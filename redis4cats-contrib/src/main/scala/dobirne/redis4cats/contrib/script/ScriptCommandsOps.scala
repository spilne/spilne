package dobirne.redis4cats.contrib.script

import cats.effect.kernel.Concurrent
import dev.profunktor.redis4cats.algebra.ScriptCommands
import dev.profunktor.redis4cats.effects.ScriptOutputType

final class ScriptCommandsOps[F[_], K, V](private val cmd: ScriptCommands[F, K, V]) extends AnyVal {

  def cacheScript(script: String)(implicit F: Concurrent[F]): F[CachedScript[F, K, V]] =
    CachedScript(cmd)(script)

  def cacheScript[Out](
    script: String,
    out: ScriptOutputType.Aux[V, Out]
  )(implicit
    F: Concurrent[F]
  ): F[CachedScriptWithFixedOutputType[F, K, V, Out]] =
    CachedScript.fixedOutputType(cmd)(script, out)

}

trait ScriptCommandsSyntax {

  final implicit def toScriptCommandsOps[F[_], K, V](cmd: ScriptCommands[F, K, V]): ScriptCommandsOps[F, K, V] =
    new ScriptCommandsOps(cmd)
}
