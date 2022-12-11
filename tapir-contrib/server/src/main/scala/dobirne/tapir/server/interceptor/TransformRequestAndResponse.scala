package dobirne.tapir.server.interceptor

import sttp.monad.MonadError
import sttp.tapir.server.interceptor.RequestResult
import sttp.tapir.server.interceptor.RequestResult.Response

abstract class TransformRequestAndResponse[F[_]: MonadError, A] extends TransformRequestAndRequestResult[F, A] {

  protected def transformResponse[B](a: A, response: Response[B]): F[RequestResult[B]]

  override def transformRequestResult[B](a: A, res: RequestResult[B]): F[RequestResult[B]] = {
    res match {
      case response: Response[B] => transformResponse(a, response)
      case failures => MonadError[F].unit(failures)
    }
  }
}
