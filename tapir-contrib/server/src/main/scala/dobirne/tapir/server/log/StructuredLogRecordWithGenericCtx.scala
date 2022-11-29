package dobirne.tapir.server.log

import sttp.tapir.AnyEndpoint
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.interceptor.log.ExceptionContext
import sttp.tapir.server.interceptor.{DecodeFailureContext, DecodeSuccessContext, SecurityFailureContext}
import sttp.tapir.server.model.ServerResponse

abstract class StructuredLogRecordWithGenericCtx[T](underlying: ServerLogFormatter[StructuredLogRecord, T])
  extends ServerLogFormatter[StructuredLogRecord, T] {

  protected def genericLogRecordContext(
    req: ServerRequest,
    endpoint: Option[AnyEndpoint]
  ): Map[String, String]

  override def formatWhenReceived(req: ServerRequest): StructuredLogRecord = {
    underlying
      .formatWhenReceived(req)
      .addContext(genericLogRecordContext(req, None))
  }

  override def formatDecodeFailureNotHandled(ctx: DecodeFailureContext, token: T): StructuredLogRecord = {
    underlying
      .formatDecodeFailureNotHandled(ctx, token)
      .addContext(genericLogRecordContext(ctx.request, Some(ctx.endpoint)))
  }

  override def formatDecodeFailureHandled(
    ctx: DecodeFailureContext,
    resp: ServerResponse[_],
    token: T): StructuredLogRecord = {
    underlying
      .formatDecodeFailureHandled(ctx, resp, token)
      .addContext(genericLogRecordContext(ctx.request, Some(ctx.endpoint)))
  }

  override def formatSecurityFailureHandled[F[_]](
    ctx: SecurityFailureContext[F, _],
    resp: ServerResponse[_],
    token: T): StructuredLogRecord = {
    underlying
      .formatSecurityFailureHandled(ctx, resp, token)
      .addContext(genericLogRecordContext(ctx.request, Some(ctx.endpoint)))
  }

  override def formatRequestHandled[F[_]](
    ctx: DecodeSuccessContext[F, _, _, _],
    resp: ServerResponse[_],
    token: T): StructuredLogRecord = {
    underlying
      .formatRequestHandled(ctx, resp, token)
      .addContext(genericLogRecordContext(ctx.request, Some(ctx.endpoint)))
  }

  override def formatException(ctx: ExceptionContext[_, _], e: Throwable, token: T): StructuredLogRecord = {
    underlying
      .formatException(ctx, e, token)
      .addContext(genericLogRecordContext(ctx.request, Some(ctx.endpoint)))
  }
}
