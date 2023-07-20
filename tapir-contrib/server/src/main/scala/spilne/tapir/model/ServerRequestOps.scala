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

package spilne.tapir.model

import sttp.model.Header
import sttp.tapir.model.ServerRequest

final class ServerRequestOps(private val req: ServerRequest) extends AnyVal {

  @inline def modifyHeaders(f: Seq[Header] => Seq[Header]): ServerRequest = {
    req.withOverride(
      headersOverride = Some(f(req.headers)),
      connectionInfoOverride = None,
      protocolOverride = None
    )
  }

  @inline def addHeader(h: Header): ServerRequest = modifyHeaders(_ :+ h)
}

trait ServerRequestSyntax {

  final implicit def toServerRequestOps(req: ServerRequest): ServerRequestOps =
    new ServerRequestOps(req)
}
