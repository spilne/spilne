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

import cats.effect.Unique.Token
import cats.effect.{Resource, Temporal}
import cats.effect.std.Queue
import cats.implicits._

import scala.concurrent.duration.DurationInt

case class UnitBuilder[F[_]: Temporal, In](
  maybeBatcher: Option[Batcher[F, Request.Void[F, In]]] = None,
  maybeQueue: Option[Resource[F, Queue[F, Request.Void[F, In]]]] = None
) extends BuilderDefaults[F, In, Unit] {

  def withBatcher(batcher: Batcher[F, Request.Void[F, In]]): UnitBuilder[F, In] =
    copy(maybeBatcher = Some(batcher))

  def withQueue(queue: Resource[F, Queue[F, Request.Void[F, In]]]): UnitBuilder[F, In] =
    copy(maybeQueue = Some(queue))

  def withExecutor(executor: fs2.Chunk[Request.Void[F, In]] => F[Unit]): Resource[F, In => F[Unit]] = {
    BatchService.unit(
      queueProvider = maybeQueue.getOrElse(defaultQueue),
      batcher = maybeBatcher.getOrElse(defaultBatcher),
      execute = executor
    )
  }
}

case class ArbitraryBuilder[F[_]: Temporal, Id, In, Res](
  maybeBatcher: Option[Batcher[F, Request[F, In, Res]]] = None,
  maybeQueue: Option[Resource[F, Queue[F, Request[F, In, Res]]]] = None,
  idGenerator: F[IdGenerator[F, Id, In]]
) extends BuilderDefaults[F, In, Res] {

  def withBatcher(batcher: Batcher[F, Request[F, In, Res]]): ArbitraryBuilder[F, Id, In, Res] =
    copy(maybeBatcher = Some(batcher))

  def withQueue(queue: Resource[F, Queue[F, Request[F, In, Res]]]): ArbitraryBuilder[F, Id, In, Res] =
    copy(maybeQueue = Some(queue))

  def withIdGeneratorF[NewId](idGenerator: F[IdGenerator[F, NewId, In]]): ArbitraryBuilder[F, NewId, In, Res] =
    copy(idGenerator = idGenerator)

  def withIdGenerator[NewId](idGenerator: IdGenerator[F, NewId, In]): ArbitraryBuilder[F, NewId, In, Res] =
    withIdGeneratorF(idGenerator.pure[F])

  def withExecutor(executor: fs2.Chunk[(Id, Request[F, In, Res])] => F[Map[Id, Res]]): Resource[F, In => F[Res]] = {
    BatchService.apply(
      queueProvider = maybeQueue.getOrElse(defaultQueue),
      batcher = maybeBatcher.getOrElse(defaultBatcher),
      execute = executor,
      idGeneratorF = idGenerator
    )
  }
}

class BatchServiceBuilders[F[_]: Temporal] {
  def withRequest[Input]: RequestBuilder[Input] = new RequestBuilder

  class RequestBuilder[In] {
    def withResponse[Res]: ArbitraryBuilder[F, Token, In, Res] =
      ArbitraryBuilder(idGenerator = IdGenerator.unique[F].pure[F].widen)

    def withUnitResponse: UnitBuilder[F, In] =
      UnitBuilder[F, In]()
  }
}

private[batcher] class BuilderDefaults[F[_]: Temporal, In, Out] {
  protected val defaultBatcher: Batcher[F, Request[F, In, Out]] =
    _.groupWithin(50, 1.second)

  protected val defaultQueue: Resource[F, Queue[F, Request[F, In, Out]]] =
    Resource.eval(Queue.unbounded[F, Request[F, In, Out]])
}
