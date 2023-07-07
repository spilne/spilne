package spilne.foldify

import cats.{Functor}

trait Transfold[F[_], A, B] { self =>

  def apply[O](f: FoldF[F, B, O]): FoldF[F, A, O]

  def andThen[O](f: FoldF[F, B, O]): FoldF[F, A, O] = apply(f)
}

object Transfold {
  import cats.syntax.all._

  private val sentinel: Any => Any = new (Any => Any) {
    override def apply(v1: Any): Any = this
  }

  def mapF[F[_], A, B](f: A => F[B]): Transfold[F, A, B] =
    new Transfold[F, A, B] {
      override def apply[O](fold: FoldF[F, B, O]): FoldF[F, A, O] = {
        fold.contramapF(f)
      }
    }

  def map[A, B](f: A => B): Transfold[cats.Id, A, B] = mapF[cats.Id, A, B](a => f(a))

  def filterF[F[_], A](p: A => F[Boolean]): Transfold[F, A, A] =
    new Transfold[F, A, A] {
      override def apply[O](f: FoldF[F, A, O]): FoldF[F, A, O] = {
        import f.monad

        Fold.foldF[F, f.S, A, O](f.init)(
          (s, i) => p(i).ifM(f.step(s, i), s.pure),
          f.done
        )
      }
    }

  def filter[A](p: A => Boolean): Transfold[cats.Id, A, A] = filterF[cats.Id, A](a => p(a))

  def filterNotF[F[_]: Functor, A](p: A => F[Boolean]): Transfold[F, A, A] = filterF(a => Functor[F].map(p(a))(!_))
  def filterNot[A](p: A => Boolean): Transfold[cats.Id, A, A] = filterNotF[cats.Id, A](a => p(a))

  def collectF[F[_], A, B](pf: PartialFunction[A, F[B]]): Transfold[F, A, B] =
    new Transfold[F, A, B] {
      override def apply[O](f: FoldF[F, B, O]): FoldF[F, A, O] = {
        import f.monad

        Fold.foldF[F, f.S, A, O](f.init)(
          (s, i) => {
            val res = pf.applyOrElse(i, sentinel)
            if (res == sentinel) s.pure else res.asInstanceOf[F[B]].flatMap(f.step(s, _))
          },
          f.done
        )
      }
    }

  def collect[A, B](pf: PartialFunction[A, B]): Transfold[cats.Id, A, B] = collectF[cats.Id, A, B](pf)

  def takeWhileF[F[_], A](f: A => F[Boolean]): Transfold[F, A, A] =
    new Transfold[F, A, A] {
      override def apply[O](fold: FoldF[F, A, O]): FoldF[F, A, O] = {
        import fold.monad

        Fold.foldF[F, (fold.S, Boolean), A, O](fold.init.tupleRight(true))(
          (s, i) => {
            if (s._2) f(i).ifM(fold.step(s._1, i).tupleRight(true), (s._1, false).pure)
            else s.pure
          },
          s => fold.done(s._1)
        )
      }
    }

  def tapEachF[F[_], A](f: A => F[Unit]): Transfold[F, A, A] =
    new Transfold[F, A, A] {
      override def apply[O](fold: FoldF[F, A, O]): FoldF[F, A, O] = {
        import fold.monad

        Fold.foldF[F, fold.S, A, O](fold.init)(
          (s, i) => f(i) >> fold.step(s, i),
          fold.done
        )
      }
    }

  def tapEach[A](f: A => Unit): Transfold[cats.Id, A, A] = tapEachF[cats.Id, A](f)

  def takeWhile[A](f: A => Boolean): Transfold[cats.Id, A, A] = takeWhileF[cats.Id, A](f)

  def takeF[F[_], A](n: Int): Transfold[F, A, A] =
    new Transfold[F, A, A] {
      override def apply[O](fold: FoldF[F, A, O]): FoldF[F, A, O] = {
        import fold.monad

        Fold.foldF[F, (fold.S, Int), A, O](fold.init.tupleRight(n))(
          (s, i) => {
            if (s._2 > 0) fold.step(s._1, i).tupleRight(s._2 - 1)
            else s.pure
          },
          s => fold.done(s._1)
        )
      }
    }

  def take[A](n: Int): Transfold[cats.Id, A, A] = takeF[cats.Id, A](n)

  def zipWithIndexF[F[_], A]: Transfold[F, A, (A, Int)] =
    new Transfold[F, A, (A, Int)] {
      override def apply[O](f: FoldF[F, (A, Int), O]): FoldF[F, A, O] = {
        import f.monad

        Fold.foldF[F, (f.S, Int), A, O](f.init.tupleRight(0))(
          (s, i) => f.step(s._1, (i, s._2)).tupleRight(s._2 + 1),
          s => f.done(s._1)
        )
      }
    }

  def zipWithIndex[A]: Transfold[cats.Id, A, (A, Int)] = zipWithIndexF[cats.Id, A]

  def dropF[F[_], A](n: Int): Transfold[F, A, A] =
    new Transfold[F, A, A] {
      override def apply[O](f: FoldF[F, A, O]): FoldF[F, A, O] = {
        import f.monad

        Fold.foldF[F, (f.S, Int), A, O](f.init.tupleRight(n))(
          (s, i) => {
            if (s._2 > 0) f.step(s._1, i).tupleRight(s._2 - 1)
            else s.pure
          },
          s => f.done(s._1)
        )
      }
    }

  def flatMapF[CC[_], F[_], A, B](f: A => CC[B])(implicit C: CanBeFolded[CC, F]): Transfold[F, A, B] =
    new Transfold[F, A, B] {
      import CanBeFolded._

      override def apply[O](fold: FoldF[F, B, O]): FoldF[F, A, O] = {
        import fold.monad

        Fold.foldF(fold.init, fold.done) { (s, a) =>
          val stepFolder = Fold.foldLeftF[F, B, fold.S](monad.pure(s))((s, i) => fold.step(s, i))
          f(a).foldWith(stepFolder)
        }
      }
    }

  def drop[A](n: Int): Transfold[cats.Id, A, A] = dropF[cats.Id, A](n)

  final implicit class Ops[F[_], A, B](private val underlying: Transfold[F, A, B]) extends AnyVal {
    def combine[C](that: Transfold[F, B, C]): Transfold[F, A, C] =
      new Transfold[F, A, C] { override def apply[O](f: FoldF[F, C, O]): FoldF[F, A, O] = underlying(that(f)) }

    def >>[C](that: Transfold[F, B, C]): Transfold[F, A, C] = combine(that)

    def filter(p: B => F[Boolean]): Transfold[F, A, B] = combine(Transfold.filterF(p))
    def map[C](f: B => F[C]): Transfold[F, A, C] = combine(Transfold.mapF(f))
    def collect[C](f: PartialFunction[B, F[C]]): Transfold[F, A, C] = combine(Transfold.collectF(f))
    def takeWhile(p: B => F[Boolean]): Transfold[F, A, B] = combine(Transfold.takeWhileF(p))
    def take(n: Int): Transfold[F, A, B] = combine(Transfold.takeF(n))
    def zipWithIndex: Transfold[F, A, (B, Int)] = combine(Transfold.zipWithIndexF)
    def tapEach(f: B => F[Unit]): Transfold[F, A, B] = combine(Transfold.tapEachF(f))
    def drop(n: Int): Transfold[F, A, B] = combine(Transfold.dropF(n))
    def flatMap[CC[_], C](f: B => CC[C])(implicit C: CanBeFolded[CC, F]): Transfold[F, A, C] =
      combine(Transfold.flatMapF(f))
  }
}
