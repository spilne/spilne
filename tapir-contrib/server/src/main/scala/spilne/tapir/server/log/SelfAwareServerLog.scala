/*
 * Copyright 2023 spilne
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spilne.tapir.server.log

import spilne.tapir.monad.syntax.*
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
