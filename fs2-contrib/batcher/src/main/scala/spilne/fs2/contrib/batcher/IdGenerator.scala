package spilne.fs2.contrib.batcher

import cats.{Applicative, FlatMap}
import cats.effect.kernel.Ref.Make
import cats.effect.kernel.Unique
import cats.effect.kernel.Unique.Token
import cats.effect.{Ref, Sync}
import cats.implicits._

import java.util.UUID

object IdGenerator {
  def pure[F[_]: Applicative, In, Id](f: In => Id): IdGenerator[F, Id, In] = in => f(in).pure[F]

  def eval[F[_], Id](fid: F[Id]): IdGenerator[F, Id, Any] = _ => fid

  def uuid[F[_]: Sync]: IdGenerator[F, UUID, Any] = eval(Sync[F].delay(UUID.randomUUID()))

  def unique[F[_]: Unique]: IdGenerator[F, Token, Any] = eval(Unique[F].unique)

  def sequence[F[_]: Make: FlatMap, N: Numeric]: F[IdGenerator[F, N, Any]] = {
    val numeric = implicitly[Numeric[N]]

    for {
      state <- Ref[F].of(numeric.zero)
      one = numeric.one
      seq = eval(state.getAndUpdate(numeric.plus(_, one)))
    } yield seq
  }

  def id[F[_]: Applicative, In]: IdGenerator[F, In, In] = pure(identity)
}
