package dobirne.tapir.monad

import sttp.monad.MonadError

final class MonadErrorOps[F[_]](private val ME: MonadError[F]) extends AnyVal {
  def whenA[A](cond: Boolean)(fa: => F[A]): F[Unit] = if (cond) ME.map(fa)(_ => ()) else ME.unit(())
}

trait MonadErrorSyntax {
  implicit def toMonadErrorOps[F[_]](me: MonadError[F]): MonadErrorOps[F] = new MonadErrorOps(me)
}
