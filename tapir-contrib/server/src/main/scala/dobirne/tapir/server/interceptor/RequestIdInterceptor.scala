package dobirne.tapir.server.interceptor

import sttp.model.Header
import sttp.monad.MonadError
import sttp.tapir.AttributeKey
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.interceptor.{EndpointInterceptor, RequestHandler, RequestInterceptor, RequestResult, Responder}

import java.util.UUID

class RequestIdInterceptor[F[_]](
  genRequestId: => F[String],
  headerName: String = RequestIdInterceptor.RequestIdHeaderName,
  requestIdAttrKey: AttributeKey[String] = RequestIdInterceptor.RequestIdAttribute
) extends RequestInterceptor[F] {
  import sttp.monad.syntax._

  override def apply[R, B](
    responder: Responder[F, B],
    requestHandler: EndpointInterceptor[F] => RequestHandler[F, R, B]
  ): RequestHandler[F, R, B] = {
    new RequestHandler[F, R, B] {
      val next: RequestHandler[F, R, B] = requestHandler(EndpointInterceptor.noop)

      override def apply(
        request: ServerRequest,
        endpoints: List[ServerEndpoint[R, F]]
      )(implicit
        monad: MonadError[F]
      ): F[RequestResult[B]] = {
        val maybeHeader: Option[Header] = request.headers.find(_.is(headerName))

        for {
          header <- maybeHeader.fold(genRequestId.map(id => Header(headerName, id)))(monad.unit)
          requestId = header.value

          requestWithHeader = {
            if (maybeHeader.isDefined) request
            else
              request.withOverride(
                headersOverride = Some(request.headers.appended(header)),
                protocolOverride = None,
                connectionInfoOverride = None
              )
          }

          reqResult <- next(requestWithHeader.attribute(requestIdAttrKey, requestId), endpoints)

          finalReqResult = {
            reqResult match {
              case RequestResult.Response(response) => RequestResult.Response(response.addHeaders(header :: Nil))
              case failure => failure
            }
          }
        } yield finalReqResult
      }
    }
  }
}

object RequestIdInterceptor {

  val RequestIdAttribute: AttributeKey[String] = new AttributeKey("dobirne.tapir.server.interceptor.RequestId")
  val RequestIdHeaderName: String = "X-Request-ID"

  def default[F[_]: MonadError]: RequestIdInterceptorBuilder[F] = new RequestIdInterceptorBuilder[F]

  final class RequestIdInterceptorBuilder[F[_]: MonadError] {

    def apply(
      genRequestId: => F[String] = MonadError[F].eval(UUID.randomUUID().toString),
      headerName: String = RequestIdInterceptor.RequestIdHeaderName,
      requestIdAttrKey: AttributeKey[String] = RequestIdInterceptor.RequestIdAttribute
    ): RequestIdInterceptor[F] = {
      new RequestIdInterceptor[F](
        genRequestId,
        headerName,
        requestIdAttrKey
      )
    }
  }
}
