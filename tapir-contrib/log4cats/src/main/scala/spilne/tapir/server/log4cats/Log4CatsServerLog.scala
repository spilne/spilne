package spilne.tapir.server.log4cats

import spilne.tapir.server.log._
import org.typelevel.log4cats.StructuredLogger
import spilne.tapir.server.log.{
  FormattableServerLog,
  StructuredLogFormatter,
  StructuredLogRecord,
  StructuredServerLogBuilder
}

import java.time.Clock

object Log4CatsServerLog {

  def apply[F[_]](
    logger: StructuredLogger[F],
    clock: Option[Clock] = Some(Clock.systemUTC())
  ): FormattableServerLog[F, StructuredLogRecord, Long] = {
    Log4CatsServerLogBuilder[F](logger, clock).build
  }
}

abstract class Log4CatsServerLogBuilder[F[_]](logger: StructuredLogger[F]) extends StructuredServerLogBuilder[F] {
  self =>

  override type Builder[
    SLF0 <: DefaultStructuredLogFormatter,
    SLW0 <: StructuredLogWriter[F]
  ] = Log4CatsServerLogBuilder[F] {
    type SLF = SLF0
    type SLW = SLW0
  }

  override def withFormatter[SLF0 <: DefaultStructuredLogFormatter](f: SLF0): Builder[SLF0, SLW] = {
    new Log4CatsServerLogBuilder(logger) {
      override type SLF = SLF0
      override type SLW = self.SLW
      override def formatter: SLF = f
      override def writer: SLW = self.writer
      override def tokenGen: () => Long = self.tokenGen
    }
  }

  override def withWriter[SLW0 <: StructuredLogWriter[F]](f: SLW0): Builder[SLF, SLW0] =
    new Log4CatsServerLogBuilder(logger) {
      override type SLF = self.SLF
      override type SLW = SLW0
      override def formatter: SLF = self.formatter
      override def writer: SLW = f
      override def tokenGen: () => Long = self.tokenGen
    }
}

object Log4CatsServerLogBuilder {
  def apply[F[_]](
    logger: StructuredLogger[F],
    mClock: Option[Clock] = Some(Clock.systemUTC())
  ) =
    new Log4CatsServerLogBuilder(logger) {
      override type SLF = DefaultStructuredLogFormatter
      override type SLW = DefaultLog4CatsServerLogWriter[F]

      override def formatter: DefaultStructuredLogFormatter = StructuredLogFormatter.default(mClock)
      override def writer: DefaultLog4CatsServerLogWriter[F] = DefaultLog4CatsServerLogWriter[F](logger)
      override val tokenGen: () => Long = mClock.fold(() => 0L)(c => () => c.millis())
    }
}
