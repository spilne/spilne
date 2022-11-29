package dobirne.tapir.server.log

import sttp.tapir.model.ServerRequest
import sttp.tapir.server.interceptor._
import sttp.tapir.server.interceptor.log.{ExceptionContext, ServerLog}
import sttp.tapir.server.model.ServerResponse

trait FormattableServerLog[F[_], LogRecord, Token] extends ServerLog[F] {

  override type TOKEN = Token

  override def requestReceived(request: ServerRequest): F[Unit] =
    writer.doLogWhenReceived(formatter.formatWhenReceived(request))

  override def decodeFailureNotHandled(ctx: DecodeFailureContext, token: Token): F[Unit] =
    writer.doLogDecodeFailureNotHandled(formatter.formatDecodeFailureNotHandled(ctx, token))

  override def decodeFailureHandled(ctx: DecodeFailureContext, response: ServerResponse[_], token: Token): F[Unit] =
    writer.doLogDecodeFailureHandled(formatter.formatDecodeFailureHandled(ctx, response, token))

  override def securityFailureHandled(
    ctx: SecurityFailureContext[F, _],
    response: ServerResponse[_],
    token: Token): F[Unit] =
    writer.doLogSecurityFailureHandled(formatter.formatSecurityFailureHandled(ctx, response, token))

  override def requestHandled(
    ctx: DecodeSuccessContext[F, _, _, _],
    response: ServerResponse[_],
    token: Token): F[Unit] =
    writer.doLogRequestHandled(formatter.formatRequestHandled(ctx, response, token))

  override def exception(ctx: ExceptionContext[_, _], ex: Throwable, token: Token): F[Unit] =
    writer.doLogException(formatter.formatException(ctx, ex, token))

  protected def formatter: ServerLogFormatter[LogRecord, Token]
  protected def writer: ServerLogWriter[F, LogRecord]
}

object FormattableServerLog {

  def apply[F[_], Record, Token](
    serverLogFormatter: ServerLogFormatter[Record, Token],
    serverLogWriter: ServerLogWriter[F, Record],
    tokenGen: () => Token
  ): FormattableServerLog[F, Record, Token] = {
    new FormattableServerLog[F, Record, Token] {
      override protected val formatter: ServerLogFormatter[Record, Token] = serverLogFormatter
      override protected val writer: ServerLogWriter[F, Record] = serverLogWriter

      override def requestToken: Token = tokenGen()
    }
  }
}
