package framework.utils

import doobie.util.log
import doobie.util.log.{LogEvent, LogHandler}
import scribe.Scribe

/** A log handler that renders SQL statements and their arguments in a human-readable format. */
class FrameworkDoobieLogHandler[F[_]](scribe: Scribe[F]) extends LogHandler[F] {

  def renderArgs(args: List[Any]): String = {
    if (args.isEmpty) "(no arguments)"
    else
      "arguments:\n" + args.iterator.zipWithIndex
        .map { case (arg, idx) =>
          s"  [$idx]: ${arg.toString.indentLinesNFL(4)}"
        }
        .mkString("\n")
  }

  def renderSql(sql: String): String = {
    val strWithQuestionMarks = sql.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")

    // Replace all question marks with `[$argumentIndex]`
    var str = strWithQuestionMarks
    var questionMarkAt = str.indexOf('?')
    var argumentIndex = 0
    while (questionMarkAt >= 0) {
      str = str.patch(questionMarkAt, s"[$argumentIndex]", 1)
      questionMarkAt = str.indexOf('?')
      argumentIndex += 1
    }

    str
  }

  def onSuccess(evt: log.Success): F[Unit] = {
    val log.Success(sql, args, label, executionTime, processingTime) = evt

    scribe.debug(
      s"""Successful Statement Execution:
          |
          |  ${renderSql(sql)}
          |  label = $label
          |  elapsed = ${executionTime.prettyFractional} exec + ${processingTime.prettyFractional} processing (${(executionTime + processingTime).prettyFractional} total)
          |
          |${renderArgs(args)}
          |""".stripMargin
    )
  }

  def onProcessingFailure(evt: log.ProcessingFailure): F[Unit] = {
    val log.ProcessingFailure(sql, args, label, executionTime, processingTime, throwable) = evt

    scribe.error(
      s"""Failed ResultSet Processing:
         |
         |  ${renderSql(sql)}
         |
         |   elapsed = ${executionTime.prettyFractional} exec + ${processingTime.prettyFractional} processing (failed) (${(executionTime + processingTime).prettyFractional} total)
         |   label = $label
         |   failure = ${throwable.getMessage}
         |
         |${renderArgs(args)}
         |""".stripMargin
    )
  }

  def onExecFailure(evt: log.ExecFailure): F[Unit] = {
    val log.ExecFailure(sql, args, label, elapsed, throwable) = evt

    scribe.error(
      s"""Failed Statement Execution:
         |
         |  ${renderSql(sql)}
         |  elapsed = ${elapsed.prettyFractional} exec (failed)
         |  label = $label
         |  failure = ${throwable.getMessage}
         |
         |${renderArgs(args)}
         |""".stripMargin
    )
  }

  override def run(evt: LogEvent): F[Unit] = {
    evt match {
      case evt: log.Success           => onSuccess(evt)
      case evt: log.ProcessingFailure => onProcessingFailure(evt)
      case evt: log.ExecFailure       => onExecFailure(evt)
    }
  }

}
