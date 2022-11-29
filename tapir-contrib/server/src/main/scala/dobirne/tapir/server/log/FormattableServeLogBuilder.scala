package dobirne.tapir.server.log

import sttp.tapir.AnyEndpoint
import sttp.tapir.model.ServerRequest

trait FormattableServeLogBuilder[F[_], R, T] {
  type SLF <: ServerLogFormatter[R, T]
  type SLW <: ServerLogWriter[F, R]

  type Builder[
    SLF0 <: ServerLogFormatter[R, T],
    SLW0 <: ServerLogWriter[F, R]
  ] <: FormattableServeLogBuilder[F, R, T] {
    type SLF = SLF0
    type SLW = SLW0
  }

  def formatter: SLF
  def writer: SLW

  def modifyFormatter[SLF0 <: ServerLogFormatter[R, T]](f: SLF => SLF0): Builder[SLF0, SLW] =
    withFormatter(f(formatter))
  def withFormatter[SLF0 <: ServerLogFormatter[R, T]](f: SLF0): Builder[SLF0, SLW]

  def modifyWriter[SLW0 <: ServerLogWriter[F, R]](f: SLW => SLW0): Builder[SLF, SLW0] = withWriter(f(writer))
  def withWriter[SLW0 <: ServerLogWriter[F, R]](f: SLW0): Builder[SLF, SLW0]

  def tokenGen: () => T

  def build: FormattableServerLog[F, R, T] =
    FormattableServerLog(
      formatter,
      writer,
      tokenGen
    )
}

trait StructuredServerLogBuilder[F[_]] extends FormattableServeLogBuilder[F, StructuredLogRecord, Long] {

  def withGenericCtx(f: (ServerRequest, Option[AnyEndpoint]) => Map[String, String])
    : Builder[StructuredLogRecordWithGenericCtx[Long], SLW] = {
    modifyFormatter(new StructuredLogRecordWithGenericCtx[Long](_) {
      override protected def genericLogRecordContext(
        req: ServerRequest,
        endpoint: Option[AnyEndpoint]): Map[String, String] =
        f(req, endpoint)
    })
  }

  def withStaticCtx(ctx: Map[String, String]): Builder[StructuredLogRecordWithGenericCtx[Long], SLW] =
    withGenericCtx((_, _) => ctx)
}
