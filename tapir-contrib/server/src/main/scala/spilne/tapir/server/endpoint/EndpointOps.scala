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

package spilne.tapir.server.endpoint

import sttp.tapir.Endpoint

final class EndpointOps[S, I, EI, O, -R](private val endpoint: Endpoint[S, I, EI, O, R]) extends AnyVal {

  def deferredServerLogic[F[_], P](f: P => I => F[Either[EI, O]]): DeferredServerEndpoint[F, S, P, I, EI, O, R] =
    DeferredServerEndpoint(endpoint, _ => f)
}

trait EndpointSyntax {

  final implicit def toEndpointOps[S, I, EI, O, R](endpoint: Endpoint[S, I, EI, O, R]): EndpointOps[S, I, EI, O, R] =
    new EndpointOps(endpoint)
}
