package framework.prelude

import framework.sourcecode.DefinedAt
import alleycats.Empty
import framework.utils.{JSLogger, NewtypeString}

/** The name of the application used for logging. */
object JSAppName extends NewtypeString with Newtype.WithoutValidation {
  extension (n: Type) {
    def /(scope: String): Type = apply(show"$n/$scope")
  }
}
type JSAppName = JSAppName.Type

enum LogLevel derives CanEqual {

  /** Level that you get for [[console.log]]. */
  case Default

  /** Level that you get for [[console.debug]]. */
  case Debug

  /** Level that you get for [[console.info]]. */
  case Info

  /** Level that you get for [[console.warn]]. */
  case Warning

  /** Level that you get for [[console.error]]. */
  case Error
}

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
