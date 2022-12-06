/*
 * Copyright (c) 2022 the purebits contributors.
 * See the project homepage at: https://pure-bits.github.io/docs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dobirne.fs2.contrib.batcher

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
