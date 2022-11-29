package dobirne.tapir.server.log

import dobirne.tapir.monad._
import sttp.monad.MonadError
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.interceptor.{DecodeFailureContext, DecodeSuccessContext, SecurityFailureContext}
import sttp.tapir.server.interceptor.log.{ExceptionContext, ServerLog}
import sttp.tapir.server.model.ServerResponse

abstract class SelfAwareServerLog[F[_]: MonadError, T](underlying: ServerLogAux[F, T]) extends ServerLog[F] {
  type TOKEN = T

  protected def logWhenReceived: Boolean
  protected def logDecodeFailureNotHandled: Boolean
  protected def logDecodeFailureHandled: Boolean
  protected def logSecurityFailureHandled: Boolean
  protected def logRequestHandled: Boolean
  protected def logException: Boolean

  override def requestReceived(request: ServerRequest): F[Unit] =
    MonadError[F].whenA(logWhenReceived)(underlying.requestReceived(request))

  override def decodeFailureNotHandled(ctx: DecodeFailureContext, token: TOKEN): F[Unit] =
    MonadError[F].whenA(logDecodeFailureNotHandled)(underlying.decodeFailureNotHandled(ctx, token))

  override def decodeFailureHandled(ctx: DecodeFailureContext, response: ServerResponse[_], token: TOKEN): F[Unit] =
    MonadError[F].whenA(logDecodeFailureHandled)(underlying.decodeFailureHandled(ctx, response, token))

  override def securityFailureHandled(
    ctx: SecurityFailureContext[F, _],
    response: ServerResponse[_],
    token: TOKEN): F[Unit] =
    MonadError[F].whenA(logSecurityFailureHandled)(underlying.securityFailureHandled(ctx, response, token))

  override def requestHandled(
    ctx: DecodeSuccessContext[F, _, _, _],
    response: ServerResponse[_],
    token: TOKEN): F[Unit] =
    MonadError[F].whenA(logRequestHandled)(underlying.requestHandled(ctx, response, token))

  override def exception(ctx: ExceptionContext[_, _], ex: Throwable, token: TOKEN): F[Unit] =
    MonadError[F].whenA(logException)(underlying.exception(ctx, ex, token))

  override def requestToken: T = underlying.requestToken
}

case class DefaultSelfAwareServerLog[F[_]: MonadError, T](
  underlying: ServerLogAux[F, T],
  logWhenReceived: Boolean = true,
  logDecodeFailureNotHandled: Boolean = true,
  logDecodeFailureHandled: Boolean = true,
  logSecurityFailureHandled: Boolean = true,
  logRequestHandled: Boolean = true,
  logException: Boolean = true
) extends SelfAwareServerLog[F, T](underlying)
