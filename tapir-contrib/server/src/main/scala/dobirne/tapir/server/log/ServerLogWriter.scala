package dobirne.tapir.server.log

trait ServerLogWriter[F[_], LogRecord] {
  def doLogWhenReceived(record: LogRecord): F[Unit]
  def doLogDecodeFailureNotHandled(record: LogRecord): F[Unit]
  def doLogDecodeFailureHandled(record: LogRecord): F[Unit]
  def doLogSecurityFailureHandled(record: LogRecord): F[Unit]
  def doLogRequestHandled(record: LogRecord): F[Unit]
  def doLogException(record: LogRecord): F[Unit]
}
