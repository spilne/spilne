package dobirne.tapir.server.endpoint

import sttp.tapir.Endpoint

final class EndpointOps[S, I, EI, O, -R](private val endpoint: Endpoint[S, I, EI, O, R]) extends AnyVal {

  def deferredServerLogic[F[_], P](f: P => I => F[Either[EI, O]]): DeferredServerEndpoint[F, S, P, I, EI, O, R] =
    DeferredServerEndpoint(endpoint, _ => f)
}

trait EndpointSyntax {

  final implicit def toEndpointOps[S, I, EI, O, R](endpoint: Endpoint[S, I, EI, O, R]): EndpointOps[S, I, EI, O, R] =
    new EndpointOps(endpoint)
}
