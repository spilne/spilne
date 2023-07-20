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
import sttp.tapir.server.interceptor.RequestResult
import sttp.tapir.server.interceptor.RequestResult.Response

abstract class TransformRequestAndResponse[F[_]: MonadError, A] extends TransformRequestAndRequestResult[F, A] {

  protected def transformResponse[B](a: A, response: Response[B]): F[RequestResult[B]]

  override def transformRequestResult[B](a: A, res: RequestResult[B]): F[RequestResult[B]] = {
    res match {
      case response: Response[B] @unchecked => transformResponse(a, response)
      case failures => MonadError[F].unit(failures)
    }
  }
}
