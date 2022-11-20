package dobirne.redis4cats.contrib.script

import cats.effect.kernel.Concurrent
import dev.profunktor.redis4cats.algebra.ScriptCommands
import dev.profunktor.redis4cats.effects.ScriptOutputType


final class ScriptCommandsOps[F[_], K, V](private val cmd: ScriptCommands[F, K, V]) extends AnyVal {

  def loadedScript(script: String)(implicit F: Concurrent[F]): F[LoadedScript[F, K, V]] =
    LoadedScript(cmd)(script)

  def loadedScript(script: String, out: ScriptOutputType[V])(implicit F: Concurrent[F]): F[LoadedScriptWithFixedOutputType[F, K, V]] =
    LoadedScript.fixedOutputType(cmd)(script, out)

}


trait ScriptCommandsSyntax {

  final implicit def toScriptCommandsOps[F[_], K, V](cmd: ScriptCommands[F, K, V]): ScriptCommandsOps[F, K, V] =
    new ScriptCommandsOps(cmd)
}