package spilne.tapir.server.log

case class StructuredLogRecord(
  msg: String,
  context: Map[String, String] = Map.empty,
  error: Option[Throwable] = None
) {
  def addContext(c: Map[String, String]): StructuredLogRecord = copy(context = context ++ c)
}

object StructuredLogRecord {
  def apply(msg: String, context: Map[String, String], error: Throwable): StructuredLogRecord =
    StructuredLogRecord(msg, context, Some(error))
}
