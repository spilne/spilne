package spilne

package object foldify {

  type Fold[A, B] = FoldF[cats.Id, A, B]

  type ReduceF[F[_], A] = FoldF[F, A, A]
  type Reduce[A] = Fold[A, A]
}
