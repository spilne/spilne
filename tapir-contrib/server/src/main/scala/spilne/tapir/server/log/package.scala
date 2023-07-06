package spilne.tapir.server

import sttp.tapir.server.interceptor.log.ServerLog

package object log {
  type ServerLogAux[F[_], Token] = ServerLog[F] { type TOKEN = Token }

  type StructuredLogFormatter[T] = ServerLogFormatter[StructuredLogRecord, T]
  type DefaultStructuredLogFormatter = StructuredLogFormatter[Long]
  type StructuredLogWriter[F[_]] = ServerLogWriter[F, StructuredLogRecord]
}
