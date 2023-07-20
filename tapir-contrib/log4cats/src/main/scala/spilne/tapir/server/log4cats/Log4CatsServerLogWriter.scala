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

package spilne.tapir.server.log4cats

import spilne.tapir.server.log._
import org.typelevel.log4cats.extras.LogLevel
import org.typelevel.log4cats.StructuredLogger
import spilne.tapir.server.log.StructuredLogRecord

trait Log4CatsServerLogWriter[F[_]] extends StructuredLogWriter[F] {
  def logger: StructuredLogger[F]

  override def doLogWhenReceived(record: StructuredLogRecord): F[Unit] = doLog(whenReceivedLogLevel)(record)
  override def doLogDecodeFailureNotHandled(record: StructuredLogRecord): F[Unit] =
    doLog(decodeFailureNotHandledLogLevel)(record)
  override def doLogDecodeFailureHandled(record: StructuredLogRecord): F[Unit] =
    doLog(decodeFailureHandledLogLevel)(record)
  override def doLogSecurityFailureHandled(record: StructuredLogRecord): F[Unit] =
    doLog(securityFailureHandledLogLevel)(record)
  override def doLogRequestHandled(record: StructuredLogRecord): F[Unit] = doLog(requestHandledLogLevel)(record)
  override def doLogException(record: StructuredLogRecord): F[Unit] = doLog(exceptionLogLevel)(record)

  protected def whenReceivedLogLevel: LogLevel
  protected def decodeFailureNotHandledLogLevel: LogLevel
  protected def decodeFailureHandledLogLevel: LogLevel
  protected def securityFailureHandledLogLevel: LogLevel
  protected def requestHandledLogLevel: LogLevel
  protected def exceptionLogLevel: LogLevel

  private def doLog(l: LogLevel)(r: StructuredLogRecord): F[Unit] = {
    l match {
      case LogLevel.Error => r.error.fold(logger.error(r.context)(r.msg))(logger.error(r.context, _)(r.msg))
      case LogLevel.Debug => r.error.fold(logger.debug(r.context)(r.msg))(logger.debug(r.context, _)(r.msg))
      case LogLevel.Info => r.error.fold(logger.info(r.context)(r.msg))(logger.info(r.context, _)(r.msg))
      case LogLevel.Warn => r.error.fold(logger.warn(r.context)(r.msg))(logger.warn(r.context, _)(r.msg))
      case LogLevel.Trace => r.error.fold(logger.trace(r.context)(r.msg))(logger.trace(r.context, _)(r.msg))
    }
  }
}

case class DefaultLog4CatsServerLogWriter[F[_]](
  logger: StructuredLogger[F],
  whenReceivedLogLevel: LogLevel = LogLevel.Debug,
  decodeFailureNotHandledLogLevel: LogLevel = LogLevel.Debug,
  decodeFailureHandledLogLevel: LogLevel = LogLevel.Debug,
  securityFailureHandledLogLevel: LogLevel = LogLevel.Debug,
  requestHandledLogLevel: LogLevel = LogLevel.Info,
  exceptionLogLevel: LogLevel = LogLevel.Error
) extends Log4CatsServerLogWriter[F]
