package dobirne.tapir.server.endpoint

import sttp.monad.MonadError
import sttp.monad.syntax._
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint

case class SecureEndpointWithServerLogic[F[_], SECURITY_INPUT, PRINCIPAL, INPUT, ERROR_OUTPUT, OUTPUT, -R](
  endpoint: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R],
  serverLogic: MonadError[F] => PRINCIPAL => INPUT => F[Either[ERROR_OUTPUT, OUTPUT]]
) extends EndpointMetaOps { outer =>

  override def securityInput: EndpointInput[SECURITY_INPUT] = endpoint.securityInput
  override def input: EndpointInput[INPUT] = endpoint.input
  override def errorOutput: EndpointOutput[ERROR_OUTPUT] = endpoint.errorOutput
  override def output: EndpointOutput[OUTPUT] = endpoint.output
  override def info: EndpointInfo = endpoint.info

  override protected def showType: String = "SecureEndpointWithServerLogic"

  def serverSecurityLogic(f: SECURITY_INPUT => F[Either[ERROR_OUTPUT, PRINCIPAL]])
    : ServerEndpoint.Full[SECURITY_INPUT, PRINCIPAL, INPUT, ERROR_OUTPUT, OUTPUT, R, F] = {
    serverEndpoint(_ => f)
  }

  def serverSecurityLogicSuccess(f: SECURITY_INPUT => F[PRINCIPAL])
    : ServerEndpoint.Full[SECURITY_INPUT, PRINCIPAL, INPUT, ERROR_OUTPUT, OUTPUT, R, F] =
    serverEndpoint(implicit m => a => f(a).map(Right(_)))

  def serverSecurityLogicError(f: SECURITY_INPUT => F[ERROR_OUTPUT])
    : ServerEndpoint.Full[SECURITY_INPUT, PRINCIPAL, INPUT, ERROR_OUTPUT, OUTPUT, R, F] = {
    serverEndpoint(implicit m => a => f(a).map(Left(_)))
  }

  def serverSecurityLogicPure(f: SECURITY_INPUT => Either[ERROR_OUTPUT, PRINCIPAL])
    : ServerEndpoint.Full[SECURITY_INPUT, PRINCIPAL, INPUT, ERROR_OUTPUT, OUTPUT, R, F] = {
    serverEndpoint(implicit m => a => f(a).unit)
  }

  def serverSecurityLogicOption(f: SECURITY_INPUT => F[Option[PRINCIPAL]])(implicit eIsUnit: Unit =:= ERROR_OUTPUT)
    : ServerEndpoint.Full[SECURITY_INPUT, PRINCIPAL, INPUT, ERROR_OUTPUT, OUTPUT, R, F] = {
    serverEndpoint(implicit m =>
      a => f(a).map(_.fold(Left(eIsUnit.apply(())): Either[ERROR_OUTPUT, PRINCIPAL])(Right(_))))
  }

  private def serverEndpoint(f: MonadError[F] => SECURITY_INPUT => F[Either[ERROR_OUTPUT, PRINCIPAL]])
    : ServerEndpoint.Full[SECURITY_INPUT, PRINCIPAL, INPUT, ERROR_OUTPUT, OUTPUT, R, F] =
    ServerEndpoint(endpoint, f, serverLogic)
}
