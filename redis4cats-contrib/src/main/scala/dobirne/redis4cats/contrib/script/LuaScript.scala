package dobirne.redis4cats.contrib.script

import cats.effect.Ref
import cats.effect.kernel.Concurrent
import cats.effect.kernel.Unique.Token
import cats.effect.std.Semaphore
import dev.profunktor.redis4cats.algebra.ScriptCommands
import dev.profunktor.redis4cats.effects.ScriptOutputType

import scala.annotation.unused

trait Script {
  def content: String
}

trait LoadedScript[F[_], K, V] extends Script {
  def evalSha(output: ScriptOutputType[V]): F[output.R]
  def evalSha(output: ScriptOutputType[V], keys: List[K]): F[output.R]
  def evalSha(output: ScriptOutputType[V], keys: List[K], values: List[V]): F[output.R]
}

abstract class LoadedScriptWithFixedOutputType[F[_], K, V](val outputType: ScriptOutputType[V])
  extends Script {
  def evalSha: F[outputType.R]
  def evalSha(keys: List[K]): F[outputType.R]
  def evalSha(keys: List[K], values: List[V]): F[outputType.R]
}

object LoadedScript {
  private case class ScriptState(digest: String, token: Token)

  def apply[F[_]: Concurrent, K, V](scripting: ScriptCommands[F, K, V])(script: String): F[LoadedScript[F, K, V]] = {
    import cats.syntax.all._

    for {
      loadScriptMutex <- Semaphore[F](1)
      scriptState     <- Ref[F].of[Option[ScriptState]](None)

      loadedScript = {
        new LoadedScript[F, K, V] {
          override def evalSha(output: ScriptOutputType[V]): F[output.R] =
            execute(scripting.evalSha(_, output))

          override def evalSha(output: ScriptOutputType[V], keys: List[K]): F[output.R] =
            execute(scripting.evalSha(_, output, keys))

          override def evalSha(output: ScriptOutputType[V], keys: List[K], values: List[V]): F[output.R] =
            execute(scripting.evalSha(_, output, keys, values))

          override def content: String = script

          private def execute[A](command: String => F[A]): F[A] = {
            getCachedOrLoad.flatMap { script =>
              command(script.digest).recoverWith {
                case e if isScriptFlushed(e) =>
                  loadScriptMutex.permit.use { _ =>
                    scriptState.get.flatMap {
                      case Some(state) if state.token ne script.token => state.pure[F]
                      case _ => loadScript
                    }
                  }.flatMap(s => command(s.digest))
              }
            }
          }

          private def getCachedOrLoad: F[ScriptState] =
            getCachedOr(loadScriptMutex.permit.use(_ => getCachedOr(loadScript)))

          private def getCachedOr(f: => F[ScriptState]): F[ScriptState] =
            scriptState.get.flatMap(_.fold(f)(_.pure[F]))

          private def loadScript: F[ScriptState] =
            scripting
              .scriptLoad(script)
              .map(ScriptState(_, new Token()))
              .flatTap(s => scriptState.set(s.some))

          // todo
          private def isScriptFlushed(@unused e: Throwable): Boolean = false
        }
      }
    } yield loadedScript
  }

  def fixedOutputType[F[_]: Concurrent, K, V, Out <: ScriptOutputType[V]](
    scripting: ScriptCommands[F, K, V]
  )(
    script: String,
    output: Out
  ): F[LoadedScriptWithFixedOutputType[F, K, V]] = {
    import cats.syntax.all._

    LoadedScript(scripting)(script).map { loadedScript =>
      new LoadedScriptWithFixedOutputType[F, K, V](output) {
        override def evalSha: F[outputType.R] = loadedScript.evalSha(outputType)
        override def evalSha(keys: List[K]): F[outputType.R] = loadedScript.evalSha(outputType, keys)
        override def evalSha(keys: List[K], values: List[V]): F[outputType.R] =
          loadedScript.evalSha(outputType, keys, values)
        override def content: String = script
      }
    }
  }
}
