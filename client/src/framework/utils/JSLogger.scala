package framework.utils

import framework.sourcecode.DefinedAt

trait JSLogger { self =>

  /** Logs a message at the specified level. */
  def at(level: LogLevel, msg: Any*)(using definedAt: DefinedAt, appName: JSAppName = JSAppName("app")): Unit =
    atScope(level, scope, msg*)

  /** Logs a message at the specified level and scope. */
  def atScope(level: LogLevel, scope: Option[String], msg: Any*)(using
    definedAt: DefinedAt,
    appName: JSAppName = JSAppName("app"),
  ): Unit

  /** Logs a message using the default Javascript logging. */
  def apply(msg: Any*)(using definedAt: DefinedAt, appName: JSAppName = JSAppName("app")): Unit =
    self.at(LogLevel.Default, msg*)

  /** Logs a debug message. */
  def debug(msg: Any*)(using definedAt: DefinedAt, appName: JSAppName = JSAppName("app")): Unit =
    self.at(LogLevel.Debug, msg*)

  /** Logs an info message. */
  def info(info: Any*)(using definedAt: DefinedAt, appName: JSAppName = JSAppName("app")): Unit =
    self.at(LogLevel.Info, info*)

  /** Logs a warning message. */
  def warning(warning: Any*)(using definedAt: DefinedAt, appName: JSAppName = JSAppName("app")): Unit =
    self.at(LogLevel.Warning, warning*)

  /** Logs an error message. */
  def error(err: Any*)(using definedAt: DefinedAt, appName: JSAppName = JSAppName("app")): Unit =
    self.at(LogLevel.Error, err*)

  /** The scope of the logger. Returns `None` if not scoped. */
  def scope: Option[String]

  /** Adds scope to the logger. */
  def scoped(scope: String): JSLogger = {
    val s = scope
    new {
      override val scope: Option[String] = Some(self.scope.fold2(s, current => show"$current/$s"))

      override def atScope(level: LogLevel, scope: Option[String], msg: Any*)(using
        definedAt: DefinedAt,
        appName: JSAppName,
      ): Unit =
        self.atScope(level, scope, msg*)
    }
  }
}
object JSLogger {

  /** Uses the [[console]] object for logging. */
  trait DefaultJavascriptLogging extends JSLogger {

    override def atScope(level: LogLevel, scope: Option[String], msg: Any*)(using
      definedAt: DefinedAt,
      appName: JSAppName,
    ): Unit = {
      val prefix = scope.fold2(appName.unwrap, scope => show"$appName/$scope")

      level match {
        case LogLevel.Default =>
          console.log(show"[$prefix] ", msg*)
          console.log(show"[$prefix]   @ $definedAt")
        case LogLevel.Debug =>
          console.debug(show"[$prefix] ", msg*)
          console.debug(show"[$prefix]   @ $definedAt")
        case LogLevel.Info =>
          console.info(show"[$prefix] ", msg*)
          console.info(show"[$prefix]   @ $definedAt")
        case LogLevel.Warning =>
          console.warn(show"[$prefix] ", msg*)
          console.warn(show"[$prefix]   @ $definedAt")
        case LogLevel.Error =>
          console.error(show"[$prefix] ", msg*)
          console.error(show"[$prefix]   @ $definedAt")
      }
    }
  }
}

/** Mix me in to a class to get a logger. */
trait WithLogger {
  given enclosingForLogger: sourcecode.Enclosing = compiletime.deferred

  lazy val log = framework.prelude.log.scoped {
    // `enclosingForLogger` will be in format of "foo.bar.Baz.enclosingForLogger", so we want to get "Baz"
    val parts = enclosingForLogger.value.split("\\.")
    if (parts.length < 2) "<unknown>" else parts(parts.length - 2)
  }
}
