package spilne.tapir.server.interceptor

import sttp.model.Header
import sttp.monad.MonadError
import sttp.tapir.AttributeKey
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.interceptor.RequestResult
import sttp.tapir.server.interceptor.RequestResult.Response

import java.util.UUID

class RequestIdInterceptor[F[_]: MonadError](
  genRequestId: => F[String],
  headerName: String = RequestIdInterceptor.RequestIdHeaderName,
  requestIdAttrKey: AttributeKey[String] = RequestIdInterceptor.RequestIdAttribute
) extends TransformRequestAndResponse[F, Header] {
  import spilne.tapir._
  import sttp.monad.syntax._

  override def transformRequest(req: ServerRequest): F[(Header, ServerRequest)] = {
    for {
      maybeHeader <- req.headers.find(_.is(headerName)).unit
      header      <- maybeHeader.fold(genRequestId.map(id => Header(headerName, id)))(_.unit)
      requestId = header.value
      requestWithHeader = if (maybeHeader.isEmpty) req.addHeader(header) else req
      requestWithHeaderAndAttribute = requestWithHeader.attribute(requestIdAttrKey, requestId)
    } yield (header, requestWithHeaderAndAttribute)
  }

  override def transformResponse[B](header: Header, res: Response[B]): F[RequestResult[B]] = {
    MonadError[F].unit(Response(res.response.addHeaders(header :: Nil)))
  }
}

object RequestIdInterceptor {

  val RequestIdAttribute: AttributeKey[String] = new AttributeKey("spilne.tapir.server.interceptor.RequestId")
  val RequestIdHeaderName: String = "X-Request-ID"

  def apply[F[_]: MonadError]: RequestIdInterceptorBuilder[F] = new RequestIdInterceptorBuilder[F]

  final class RequestIdInterceptorBuilder[F[_]: MonadError] {

    def default(
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
