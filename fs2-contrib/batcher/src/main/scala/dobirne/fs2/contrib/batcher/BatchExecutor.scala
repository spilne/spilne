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

import cats.MonadError
import cats.implicits._

private[batcher] abstract class BatchExecutor[F[_], Batch](implicit F: MonadError[F, Throwable]) {
  type Out

  def apply(batch: Batch): F[Unit] = {
    execute(batch).attempt
      .flatMap(handleResult(batch, _))
  }

  protected def execute(batch: Batch): F[Out]
  protected def handleResult(batch: Batch, errorOrOut: Either[Throwable, Out]): F[Unit]
}

private[batcher] object BatchExecutor {

  def apply[F[_], Batch, BatchOut](
    _execute: Batch => F[BatchOut],
    _handleResult: (Batch, Either[Throwable, BatchOut]) => F[Unit]
  )(implicit F: MonadError[F, Throwable]): BatchExecutor[F, Batch] =
    new BatchExecutor[F, Batch] {
      type Out = BatchOut

      override protected def execute(batch: Batch): F[BatchOut] =
        _execute(batch)

      override protected def handleResult(batch: Batch, errorOrOut: Either[Throwable, BatchOut]): F[Unit] =
        _handleResult(batch, errorOrOut)
    }

  def unit[F[_], In](
    execute: fs2.Chunk[Request[F, In, Unit]] => F[Unit]
  )(implicit
    F: MonadError[F, Throwable]
  ): BatchExecutor[F, fs2.Chunk[Request[F, In, Unit]]] = {
    apply[F, fs2.Chunk[Request[F, In, Unit]], Unit](
      execute,
      (batch, errorOrOut) => batch.traverse_(_.complete(errorOrOut))
    )
  }

  def withCorrelationId[F[_], Id, In, Res](
    execute: fs2.Chunk[(Id, Request[F, In, Res])] => F[Map[Id, Res]]
  )(implicit
    F: MonadError[F, Throwable]
  ): BatchExecutor[F, fs2.Chunk[(Id, Request[F, In, Res])]] = {
    val notFoundError = new NoSuchElementException("response not found")

    apply[F, fs2.Chunk[(Id, Request[F, In, Res])], Map[Id, Res]](
      execute,
      (batch, errorOrOut) =>
        errorOrOut match {
          case e: Left[Throwable, _] =>
            batch.traverse_ { case (_, req) => req.complete(e.rightCast) }

          case Right(idToRes) =>
            batch.traverse_ {
              case (id, req) =>
                req.complete(idToRes.get(id).toRight(notFoundError))
            }
        }
    )
  }
}
