package framework.prelude

import framework.sourcecode.DefinedAt
import alleycats.Empty
import framework.utils.{JSLogger, LogLevel, NewtypeString}

/** The name of the application used for logging. */
object JSAppName extends NewtypeString with Newtype.WithoutValidation {
  extension (n: Type) {
    def /(scope: String): Type = apply(show"$n/$scope")
  }
}
type JSAppName = JSAppName.Type

object log extends JSLogger.DefaultJavascriptLogging {
  override def scope: Option[String] = None
}

extension [L, R](either: Either[L, R]) {

  /** Gets the right value or logs at the specified level and returns the empty value. */
  def getOrLogAndEmpty(
    level: LogLevel
  )(using definedAt: DefinedAt, empty: Empty[R], appName: JSAppName = JSAppName("app")): R = {
    either match {
      case Right(value) => value
      case Left(error) =>
        log.at(level, error)
        empty.empty
    }
  }
}
