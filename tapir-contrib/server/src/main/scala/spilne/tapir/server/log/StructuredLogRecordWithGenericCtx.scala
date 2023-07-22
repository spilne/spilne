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

  override def formatWhenReceived(req: ServerRequest, token: T): StructuredLogRecord = {
    underlying
      .formatWhenReceived(req, token)
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
