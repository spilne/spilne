package dobirne.redis4cats.contrib.script

import cats.Functor
import cats.syntax.all._
import cats.effect.kernel.Concurrent
import dev.profunktor.redis4cats.algebra.Scripting
import dev.profunktor.redis4cats.effects.ScriptOutputType

final class ScriptCommandsOps[F[_], K, V](private val cmd: Scripting[F, K, V]) extends AnyVal {

  def script[O](content: String, out: ScriptOutputType.Aux[V, O]): Script[F, K, V, O] =
    Script(cmd)(content, out)

  def cacheScript[O](script: Script[F, K, V, O])(implicit F: Concurrent[F]): F[CachedScript[F, K, V, O]] =
    CachedScript(cmd)(script)

  def cacheScript[O](
    content: String,
    out: ScriptOutputType.Aux[V, O]
  )(implicit
    F: Concurrent[F]
  ): F[CachedScript[F, K, V, O]] =
    cacheScript(script(content, out))

  def scriptCached(digest: String)(implicit F: Functor[F]): F[Boolean] =
    cmd.scriptExists(digest).map(_.headOption.getOrElse(false))
}

trait ScriptCommandsSyntax {

  final implicit def toScriptCommandsOps[F[_], K, V](cmd: Scripting[F, K, V]): ScriptCommandsOps[F, K, V] =
    new ScriptCommandsOps(cmd)
}
