package dobirne.fs2.contrib.batcher

import cats.implicits._
import cats.effect.{Deferred, Resource}
import cats.effect.kernel.{Concurrent, Temporal, Unique}
import cats.effect.std.Queue

object BatchService {

  def apply[F[_]: Temporal]: BatchServiceBuilders[F] =
    new BatchServiceBuilders[F]

  def apply[F[_]: Temporal, In, Out]: ArbitraryBuilder[F, Unique.Token, In, Out] =
    BatchService[F]
      .withRequest[In]
      .withResponse[Out]

  def void[F[_]: Temporal, In]: UnitBuilder[F, In] =
    BatchService[F]
      .withRequest[In]
      .withUnitResponse

  def apply[F[_]: Temporal, Id, In, Out](
    execute: fs2.Chunk[(Id, Request[F, In, Out])] => F[Map[Id, Out]],
    batcher: Batcher[F, Request[F, In, Out]],
    queueProvider: Resource[F, Queue[F, Request[F, In, Out]]],
    idGeneratorF: F[IdGenerator[F, Id, In]]
  ): Resource[F, In => F[Out]] = {
    Resource
      .eval(idGeneratorF)
      .flatMap { idGenerator =>
        apply(
          execute,
          batcher.andThen(_.mapAsyncUnordered(Int.MaxValue)(_.traverse(req => idGenerator(req.data).tupleRight(req)))),
          queueProvider
        )
      }
  }

  def apply[F[_]: Temporal, Id, In, Out](
    execute: fs2.Chunk[(Id, Request[F, In, Out])] => F[Map[Id, Out]],
    batcher: fs2.Pipe[F, Request[F, In, Out], fs2.Chunk[(Id, Request[F, In, Out])]],
    queueProvider: Resource[F, Queue[F, Request[F, In, Out]]]
  ): Resource[F, In => F[Out]] = {
    instance[F, In, Out, fs2.Chunk[(Id, Request[F, In, Out])]](
      batcher,
      BatchExecutor.withCorrelationId(execute),
      queueProvider
    )
  }

  def unit[F[_]: Temporal, In](
    batcher: Batcher[F, Request.Void[F, In]],
    execute: fs2.Chunk[Request.Void[F, In]] => F[Unit],
    queueProvider: Resource[F, Queue[F, Request.Void[F, In]]]
  ): Resource[F, In => F[Unit]] = {
    instance(
      batcher,
      BatchExecutor.unit(execute),
      queueProvider
    )
  }

  private def instance[F[_]: Concurrent, In, Out, Batch](
    batcher: fs2.Pipe[F, Request[F, In, Out], Batch],
    execute: BatchExecutor[F, Batch],
    queueProvider: Resource[F, Queue[F, Request[F, In, Out]]]
  ): Resource[F, In => F[Out]] = {
    type ErrorOrRes = Either[Throwable, Out]

    def mkRequest(in: In, deferred: Deferred[F, ErrorOrRes]): Request[F, In, Out] =
      new Request[F, In, Out](in) {
        override private[batcher] def complete(res: ErrorOrRes): F[Unit] =
          deferred.complete(res).void
      }

    queueProvider.flatMap { queue =>
      val svc: In => F[Out] = in =>
        for {
          deferred <- Deferred[F, ErrorOrRes]
          request = mkRequest(in, deferred)
          _        <- queue.offer(request)
          response <- deferred.get.flatMap(_.liftTo[F])
        } yield response

      val executor: fs2.Stream[F, Nothing] =
        fs2.Stream
          .repeatEval(queue.take)
          .through(batcher)
          .mapAsyncUnordered(Int.MaxValue)(execute(_))
          .drain

      fs2.Stream
        .emit(svc)
        .concurrently(executor)
        .compile
        .resource
        .lastOrError
    }
  }
}
