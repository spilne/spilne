package dobirne.tapir.server.log

import sttp.tapir.{AnyEndpoint, DecodeResult}
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.interceptor.log.ExceptionContext
import sttp.tapir.server.interceptor.{DecodeFailureContext, DecodeSuccessContext, SecurityFailureContext}
import sttp.tapir.server.model.ServerResponse

import java.time.Clock

trait ServerLogFormatter[LogRecord, Token] {
  def formatWhenReceived(req: ServerRequest): LogRecord
  def formatDecodeFailureNotHandled(ctx: DecodeFailureContext, token: Token): LogRecord
  def formatDecodeFailureHandled(ctx: DecodeFailureContext, resp: ServerResponse[_], token: Token): LogRecord
  def formatSecurityFailureHandled[F[_]](
    ctx: SecurityFailureContext[F, _],
    resp: ServerResponse[_],
    token: Token): LogRecord
  def formatRequestHandled[F[_]](
    ctx: DecodeSuccessContext[F, _, _, _],
    resp: ServerResponse[_],
    token: Token): LogRecord
  def formatException(ctx: ExceptionContext[_, _], e: Throwable, token: Token): LogRecord
}

trait StructuredLogFormatterL extends ServerLogFormatter[StructuredLogRecord, Long] {
  def clock: Option[Clock]

  // todo
  protected def formatRequest(req: ServerRequest): String = req.showShort
  protected def formatEndpoint(endpoint: AnyEndpoint): String = endpoint.showShort
  protected def formatResponse(resp: ServerResponse[_]): String = resp.showShort

  override def formatWhenReceived(req: ServerRequest): StructuredLogRecord = {
    StructuredLogRecord(s"Request received: ${req.showShort}")
  }

  override def formatDecodeFailureNotHandled(ctx: DecodeFailureContext, token: Long): StructuredLogRecord = {
    StructuredLogRecord(
      s"Request: ${ctx.request.showShort}, not handled by: ${ctx.endpoint.showShort}${took(
        token)}; decode failure: ${ctx.failure}, on input: ${ctx.failingInput.show}",
      error = exception(ctx)
    )
  }

  override def formatDecodeFailureHandled(
    ctx: DecodeFailureContext,
    resp: ServerResponse[_],
    token: Long): StructuredLogRecord = {
    StructuredLogRecord(
      s"Request: ${ctx.request.showShort}, handled by: ${ctx.endpoint.showShort}${took(
        token)}; decode failure: ${ctx.failure}, on input: ${ctx.failingInput.show}; response: ${resp.showShort}",
      error = exception(ctx)
    )
  }

  override def formatSecurityFailureHandled[F[_]](
    ctx: SecurityFailureContext[F, _],
    resp: ServerResponse[_],
    token: Long): StructuredLogRecord = {
    StructuredLogRecord(
      s"Request: ${ctx.request.showShort}, handled by: ${ctx.endpoint.showShort}${took(
        token)}; security logic error response: ${resp.showShort}"
    )
  }

  override def formatRequestHandled[F[_]](
    ctx: DecodeSuccessContext[F, _, _, _],
    resp: ServerResponse[_],
    token: Long): StructuredLogRecord = {
    StructuredLogRecord(
      s"Request: ${ctx.request.showShort}, handled by: ${ctx.endpoint.showShort}${took(token)}; response: ${resp.showShort}"
    )
  }

  override def formatException(ctx: ExceptionContext[_, _], e: Throwable, token: Long): StructuredLogRecord = {
    StructuredLogRecord(
      s"Exception when handling request: ${ctx.request.showShort}, by: ${ctx.endpoint.showShort}${took(token)}",
      error = e
    )
  }

  private def took(token: Long): String = clock.fold("")(c => s", took: ${c.millis() - token}ms")

  private def exception(ctx: DecodeFailureContext): Option[Throwable] =
    ctx.failure match {
      case DecodeResult.Error(_, error) => Some(error)
      case _ => None
    }
}

object StructuredLogFormatter {

  def default(maybeClock: Option[Clock] = Some(Clock.systemUTC())): StructuredLogFormatterL = {
    new StructuredLogFormatterL {
      override val clock: Option[Clock] = maybeClock
    }
  }
}
