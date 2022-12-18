package dobirne.tapir.model

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
