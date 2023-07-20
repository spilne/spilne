package spilne.tapir.server.interceptor

import sttp.monad.MonadError
import sttp.monad.syntax._
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.interceptor.{EndpointInterceptor, RequestHandler, RequestInterceptor, RequestResult, Responder}

trait TransformRequestAndRequestResult[F[_], A] extends RequestInterceptor[F] {
  private val noopEndpointInterceptor = EndpointInterceptor.noop[F]

  protected def transformRequest(req: ServerRequest): F[(A, ServerRequest)]
  protected def transformRequestResult[B](a: A, res: RequestResult[B]): F[RequestResult[B]]

  override def apply[R, D](
    responder: Responder[F, D],
    requestHandler: EndpointInterceptor[F] => RequestHandler[F, R, D]
  ): RequestHandler[F, R, D] = {

    new RequestHandler[F, R, D] {

      override def apply(
        request: ServerRequest,
        endpoints: List[ServerEndpoint[R, F]]
      )(implicit
        monad: MonadError[F]
      ): F[RequestResult[D]] = {
        for {
          transformationRes <- transformRequest(request)
          (a, transformedRequest) = transformationRes
          response            <- requestHandler(noopEndpointInterceptor)(transformedRequest, endpoints)
          transformedResponse <- transformRequestResult(a, response)
        } yield transformedResponse
      }
    }
  }
}
