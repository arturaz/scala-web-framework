package framework.utils

import doobie.util.log
import doobie.util.log.{LogEvent, LogHandler, Parameters}
import scribe.Scribe

/** A log handler that renders SQL statements and their arguments in a human-readable format. */
class FrameworkDoobieLogHandler[F[_]](scribe: Scribe[F]) extends LogHandler[F] {
  given PrettyPrintDuration.Strings = PrettyPrintDuration.Strings.EnglishShortNoSpaces

  def renderArgs(params: Parameters): String = {
    def argsToString(args: List[Any], prefix: String) =
      args.iterator.zipWithIndex
        .map { case (arg, idx) =>
          s"$prefix[${idx + 1}]: ${arg.toString.indentLinesNFL(2)}"
        }
        .mkString("\n")

    def argsListToString(argsList: List[List[Any]]) =
      argsList.iterator.zipWithIndex
        .map { case (args, idx) =>
          s"""|[entry #$idx]:
              |  ${argsToString(args, prefix = "arg ").indentLinesNFL(2)}""".stripMargin
        }
        .mkString("\n")

    params match {
      case Parameters.NonBatch(args) =>
        if (args.isEmpty) "(no arguments)" else show"arguments:\n${argsToString(args, prefix = "").indentLines(2)}"

      case Parameters.Batch(argsListFn) =>
        val argsList = argsListFn()
        if (argsList.isEmpty) "(no arguments)"
        else show"batch arguments:\n${argsListToString(argsList).indentLines(2)}"
    }
  }

  def renderSql(sql: String): String = {
    val strWithQuestionMarks = sql.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")

    // Replace all question marks with `[$argumentIndex]`
    var str = strWithQuestionMarks
    var questionMarkAt = str.indexOf('?')
    var argumentIndex = 0
    while (questionMarkAt >= 0) {
      str = str.patch(questionMarkAt, s"[${argumentIndex + 1}]", 1)
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
          |  elapsed = ${executionTime.prettyForDebug} exec + ${processingTime.prettyForDebug} processing (${(executionTime + processingTime).prettyForDebug} total)
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
         |   elapsed = ${executionTime.prettyForDebug} exec + ${processingTime.prettyForDebug} processing (failed) (${(executionTime + processingTime).prettyForDebug} total)
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
         |  elapsed = ${elapsed.prettyForDebug} exec (failed)
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
