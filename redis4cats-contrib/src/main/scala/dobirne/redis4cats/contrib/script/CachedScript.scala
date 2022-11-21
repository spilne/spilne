package dobirne.redis4cats.contrib.script

import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.effect.kernel.Unique.Token
import cats.effect.std.Semaphore
import dev.profunktor.redis4cats.algebra.ScriptCommands
import dev.profunktor.redis4cats.effects.ScriptOutputType
import io.lettuce.core.RedisNoScriptException

trait Script[F[_]] {
  def content: String

  def digest: F[String]
}

trait CachedScript[F[_], K, V] extends Script[F] {
  def evalSha(output: ScriptOutputType[V]): F[output.R]
  def evalSha(output: ScriptOutputType[V], keys: List[K]): F[output.R]
  def evalSha(output: ScriptOutputType[V], keys: List[K], values: List[V]): F[output.R]
}

trait CachedScriptWithFixedOutputType[F[_], K, V, Out] extends Script[F] {
  def outputType: ScriptOutputType.Aux[V, Out]
  def evalSha: F[Out]
  def evalSha(keys: List[K]): F[Out]
  def evalSha(keys: List[K], values: List[V]): F[Out]
}

object CachedScript {
  import cats.syntax.all._

  private case class ScriptToken(digest: String, token: Token)

  def apply[F[_]: Concurrent, K, V](scripting: ScriptCommands[F, K, V])(script: String): F[CachedScript[F, K, V]] = {
    def loadScript: F[ScriptToken] =
      scripting
        .scriptLoad(script)
        .map(ScriptToken(_, new Token()))

    for {
      loadScriptMutex <- Semaphore[F](1)
      scriptTokenRef  <- Ref.ofEffect(loadScript)

      cachedScript = {
        new CachedScript[F, K, V] {
          override def evalSha(output: ScriptOutputType[V]): F[output.R] =
            execute(scripting.evalSha(_, output))

          override def evalSha(output: ScriptOutputType[V], keys: List[K]): F[output.R] =
            execute(scripting.evalSha(_, output, keys))

          override def evalSha(output: ScriptOutputType[V], keys: List[K], values: List[V]): F[output.R] =
            execute(scripting.evalSha(_, output, keys, values))

          override val content: String = script

          override val digest: F[String] = scriptTokenRef.get.map(_.digest)

          private def execute[A](command: String => F[A]): F[A] = {
            scriptTokenRef.get.flatMap { script =>
              val executeScript: F[A] = command(script.digest)

              executeScript.recoverWith {
                case _: RedisNoScriptException =>
                  val reloadIfNeeded: F[Unit] = loadScriptMutex.permit.surround(
                    for {
                      shouldReload <- scriptTokenRef.get.map(_.token eq script.token)
                      _            <- (loadScript >>= scriptTokenRef.set).whenA(shouldReload)
                    } yield ()
                  )

                  reloadIfNeeded >> executeScript
              }
            }
          }
        }
      }
    } yield cachedScript
  }

  def fixedOutputType[F[_]: Concurrent, K, V, Out](
    scripting: ScriptCommands[F, K, V]
  )(
    script: String,
    output: ScriptOutputType.Aux[V, Out]
  ): F[CachedScriptWithFixedOutputType[F, K, V, Out]] = {

    CachedScript(scripting)(script).map { cachedScript =>
      new CachedScriptWithFixedOutputType[F, K, V, Out] {
        override def outputType: ScriptOutputType.Aux[V, Out] = output
        override def evalSha: F[Out] = cachedScript.evalSha(outputType)
        override def evalSha(keys: List[K]): F[Out] = cachedScript.evalSha(outputType, keys)
        override def evalSha(keys: List[K], values: List[V]): F[Out] =
          cachedScript.evalSha(outputType, keys, values)
        override def content: String = cachedScript.content
        override def digest: F[String] = cachedScript.digest
      }
    }
  }
}
