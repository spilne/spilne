package spilne.fs2.contrib.batcher

abstract case class Request[F[_], In, Out] private[batcher] (data: In) {

  private[batcher] def complete(out: Either[Throwable, Out]): F[Unit]
}

object Request {
  type Void[F[_], In] = Request[F, In, Unit]
}
