package dobirne.redis4cats.contrib.script

import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.effect.kernel.Unique.Token
import cats.effect.std.Semaphore
import dev.profunktor.redis4cats.algebra.Scripting
import dev.profunktor.redis4cats.effects.ScriptOutputType.Aux
import io.lettuce.core.RedisNoScriptException

trait CachedScript[F[_], K, V, O] extends Script[F, K, V, O] {
  def digest: F[String]
  def origin: Script[F, K, V, O]
}

object CachedScript {
  import cats.syntax.all._

  private case class ScriptToken(digest: String, token: Token)

  def apply[F[_]: Concurrent, K, V, O](scripting: Scripting[F, K, V])(
    script: Script[F, K, V, O]): F[CachedScript[F, K, V, O]] = {
    def loadScript: F[ScriptToken] =
      scripting
        .scriptLoad(script.content)
        .map(ScriptToken(_, new Token()))

    for {
      loadScriptMutex <- Semaphore[F](1)
      scriptTokenRef  <- Ref.ofEffect(loadScript)

      cachedScript = {
        new CachedScript[F, K, V, O] {

          override def digest: F[String] = scriptTokenRef.get.map(_.digest)

          override def eval(args: ScriptArgs[K, V]): F[O] =
            execute {
              args match {
                case ScriptArgs(Nil, Nil) => scripting.evalSha(_, outputType)
                case ScriptArgs(keys, Nil) => scripting.evalSha(_, outputType, keys)
                case ScriptArgs(keys, values) => scripting.evalSha(_, outputType, keys, values)
              }
            }

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

          override def origin: Script[F, K, V, O] = script
          override def content: String = script.content
          override def outputType: Aux[V, O] = script.outputType
        }
      }
    } yield cachedScript
  }
}
