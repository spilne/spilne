package spilne.foldify

import cats.arrow.Profunctor
import cats.syntax.all._
import cats.{~>, Applicative, Eq, Monad, Monoid, Semigroup}

import scala.collection.{mutable, Factory, IterableFactory}

trait FoldF[F[_], I, O] { self =>
  type S

  def init: F[S]
  def step(s: S, i: I): F[S]
  def done(s: S): F[O]

  implicit protected[foldify] def monad: Monad[F]

  def map[O2](f: O => O2): FoldF[F, I, O2] =
    Fold.foldF(self.init)(self.step, s => self.done(s).map(f))

  def contramap[I2](f: I2 => I): FoldF[F, I2, O] =
    Fold.foldF(self.init)((s, i2) => self.step(s, f(i2)), self.done)

  def dimap[I2, O2](f: I2 => I)(g: O => O2): FoldF[F, I2, O2] =
    Fold.foldF(self.init)((s, i2) => self.step(s, f(i2)), s => self.done(s).map(g))

  def mapF[O1](f: O => F[O1]): FoldF[F, I, O1] =
    Fold.foldF(self.init)(self.step, s => self.done(s).flatMap(f))

  def contramapF[I2](f: I2 => F[I]): FoldF[F, I2, O] =
    Fold.foldF(self.init)((s, i2) => f(i2).flatMap(self.step(s, _)), self.done)

  def dimapF[I2, O1](f: I2 => F[I])(g: O => F[O1]): FoldF[F, I2, O1] =
    Fold.foldF(self.init)((s, i2) => f(i2).flatMap(self.step(s, _)), s => self.done(s).flatMap(g))

  def as[O2](o2: O2): FoldF[F, I, O2] = map(_ => o2)

  def void: FoldF[F, I, Unit] = as(())

  def mapK[G[+_]](implicit fk: F ~> G, M: Monad[G]): FoldF[G, I, O] =
    Fold.foldF(fk(self.init))((s, i) => fk(self.step(s, i)), s => fk(self.done(s)))

  def leftNarrow[I1 <: I]: FoldF[F, I1, O] = this.asInstanceOf[FoldF[F, I1, O]]
  def rightWiden[O1 >: O]: FoldF[F, I, O1] = this.asInstanceOf[FoldF[F, I, O1]]
}

trait FoldInstances {

  implicit def foldProfunctor[F[_]]: Profunctor[FoldF[F, *, *]] =
    new Profunctor[FoldF[F, *, *]] {
      override def dimap[A, B, C, D](fab: FoldF[F, A, B])(f: C => A)(g: B => D): FoldF[F, C, D] = fab.dimap(f)(g)
      override def lmap[A, B, C](fab: FoldF[F, A, B])(f: C => A): FoldF[F, C, B] = fab.contramap(f)
      override def rmap[A, B, C](fab: FoldF[F, A, B])(f: B => C): FoldF[F, A, C] = fab.map(f)
    }

  implicit def foldApplicative[F[_]: Monad, A]: Applicative[FoldF[F, A, *]] =
    new Applicative[FoldF[F, A, *]] {
      override def pure[B](b: B): FoldF[F, A, B] = Fold.pure(b)

      override def ap[B, C](ff: FoldF[F, A, B => C])(fa: FoldF[F, A, B]): FoldF[F, A, C] = {
        Fold.foldF[F, (ff.S, fa.S), A, C](ff.init.product(fa.init))(
          (s, a) => ff.step(s._1, a).product(fa.step(s._2, a)),
          s => (ff.done(s._1), fa.done(s._2)).mapN { case (f, b) => f(b) }
        )
      }
    }

  implicit def fromFactory[CC[_], A](factory: Factory[A, CC[A]]): Fold[A, CC[A]] =
    Fold.foldF[cats.Id, mutable.Builder[A, CC[A]], A, CC[A]](factory.newBuilder)(_.addOne(_), _.result())

  implicit def fromIterableFactory[CC[_], A](factory: IterableFactory[CC]): Fold[A, CC[A]] =
    fromFactory[CC, A](IterableFactory.toFactory(factory))
}

trait FoldBaseConstructors {
  import cats.implicits._

  @inline def foldF[F[_]: Monad, A, B, C](initF: => F[A])(stepF: (A, B) => F[A], doneF: A => F[C]): FoldF[F, B, C] = {
    val M: Monad[F] = Monad[F]
    new FoldF[F, B, C] {
      type S = A

      override implicit protected[foldify] def monad: Monad[F] = M

      def init: F[S] = initF
      def step(a: A, b: B): F[A] = stepF(a, b)
      def done(a: A): F[C] = doneF(a)
    }
  }

  @inline def foldF[F[_]: Monad, A, B, C](initF: => F[A], doneF: A => F[C])(stepF: (A, B) => F[A]): FoldF[F, B, C] = {
    val M: Monad[F] = Monad[F]
    new FoldF[F, B, C] {
      type S = A

      override implicit protected[foldify] def monad: Monad[F] = M

      def init: F[S] = initF

      def step(a: A, b: B): F[A] = stepF(a, b)

      def done(a: A): F[C] = doneF(a)
    }
  }

  @inline def foldLeftF[F[_]: Monad, I, O](initF: => F[O])(stepF: (O, I) => F[O]): FoldF[F, I, O] =
    foldF(initF)(stepF, Monad[F].pure)

  @inline def foldLeft[I, O](init: O)(step: (O, I) => O): Fold[I, O] =
    foldLeftF[cats.Id, I, O](init)(step)

  @inline def reduceF[F[_]: Monad, I](f: (I, I) => F[I]): FoldF[F, I, Option[I]] =
    onFirstAndRest[F, I, I, Option[I]](identity(_).pure[F], f, Option(_).pure[F])

  @inline def reduce[I](f: (I, I) => I): Fold[I, Option[I]] =
    reduceF[cats.Id, I](f)

  @inline def pure[F[_]: Monad, I, O](o: O): FoldF[F, I, O] = {
    val res = Applicative[F].pure(o)
    foldLeftF(res)((_, _) => res)
  }

  @inline private[foldify] def onFirstAndRest[F[_]: Monad, S, I, O](
    onFirst: I => F[S],
    step: (S, I) => F[S],
    done: S => F[O]
  ): FoldF[F, I, O] = {
    foldF(Monad[F].pure(null.asInstanceOf[S]))(
      (s, i) => if (s == null) onFirst(i) else step(s, i),
      done
    )
  }
}

trait Folds { this: FoldBaseConstructors =>
  def max[I: Ordering]: Fold[I, Option[I]] = reduce(Ordering[I].max)

  def maxBy[A, B: Ordering](f: A => B): Fold[A, Option[A]] = max(Ordering[B].on(f))

  def min[I: Ordering]: Fold[I, Option[I]] = reduce(Ordering[I].min)

  def minBy[A, B: Ordering](f: A => B): Fold[A, Option[A]] = min(Ordering[B].on(f))

  def sum[I: Monoid]: Fold[I, I] = foldLeft(Monoid.empty[I])(Monoid.combine(_, _))

  def foldMonoid[I: Monoid]: Fold[I, I] = sum

  def foldMap[A, B: Monoid](f: A => B): Fold[A, B] = foldMonoid[B].contramap[A](f)

  def sumOption[I: Semigroup]: Fold[I, Option[I]] = reduce(Semigroup[I].combine)

  def first[I]: Fold[I, Option[I]] = sumOption(Semigroup.first)

  def last[I]: Fold[I, Option[I]] = sumOption(Semigroup.last)

  def count[I](pred: I => Boolean): Fold[I, Int] =
    Transfold.filter[I](pred).andThen(size)

  def size[I]: Fold[I, Int] = sum[Int].contramap[I](_ => 1)

  def isEmpty[I]: Fold[I, Boolean] = Fold.foldLeft(true)((_, _) => false)

  def nonEmpty[I]: Fold[I, Boolean] = Fold.foldLeft(false)((_, _) => true)

  def exists[I](pred: I => Boolean): Fold[I, Boolean] = Fold.foldLeft(false)((b, i) => b || pred(i))

  def forall[I](pred: I => Boolean): Fold[I, Boolean] = Fold.foldLeft(true)((b, i) => b && pred(i))

  def contains[I: Eq](i: I): Fold[I, Boolean] = exists(Eq.eqv(_, i))

  def doesNotContain[I: Eq](i: I): Fold[I, Boolean] = forall(Eq.neqv(_, i))
}

object Fold extends FoldBaseConstructors with FoldInstances with Folds {

  implicit class FoldFOps[F[_], I, O](private val self: FoldF[F, I, O]) extends AnyVal {
    def or[O1](default: => O1)(implicit ev: O =:= Option[O1]): FoldF[F, I, O1] = self.map(_.getOrElse(default))

    def orEmpty[O1](implicit M: Monoid[O1], ev: O =:= Option[O1]): FoldF[F, I, O1] = or(M.empty)
  }
}
