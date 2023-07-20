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
