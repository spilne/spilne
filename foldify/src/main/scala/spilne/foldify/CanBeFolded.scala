package spilne.foldify

import cats.{Foldable, Monad}

trait CanBeFolded[CC[_], F[_]] {

  def foldWith[A, B](xs: CC[A])(fold: FoldF[F, A, B]): F[B]
}

object CanBeFolded extends CanBeFoldedInstances {
  implicit final class CanBeFoldedOps[CC[_], A](private val xs: CC[A]) extends AnyVal {

    def foldWith[F[_], B](fold: FoldF[F, A, B])(implicit ev: CanBeFolded[CC, F]): F[B] =
      ev.foldWith(xs)(fold)
  }
}

trait CanBeFoldedInstances {
  import cats.syntax.all._

  implicit def foldable[CC[_]: Foldable, F[_]: Monad]: CanBeFolded[CC, F] =
    new CanBeFolded[CC, F] {
      override def foldWith[A, B](xs: CC[A])(fold: FoldF[F, A, B]): F[B] = {
        for {
          initState  <- fold.init
          finalState <- Foldable[CC].foldM(xs, initState)((s, a) => fold.step(s, a))
          result     <- fold.done(finalState)
        } yield result
      }
    }

  implicit def iterableOnce[CC[_] <: IterableOnce[_]]: CanBeFolded[CC, cats.Id] = {
    new CanBeFolded[CC, cats.Id] {
      override def foldWith[A, B](xs: CC[A])(fold: Fold[A, B]): B = {
        val iter: Iterator[A] = xs.iterator.asInstanceOf[Iterator[A]]
        fold.done(iter.foldLeft(fold.init)((s, a) => fold.step(s, a)))
      }
    }
  }
}
