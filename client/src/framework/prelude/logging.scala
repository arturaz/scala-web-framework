package framework.prelude

import framework.sourcecode.DefinedAt

/** The name of the application used for logging. */
opaque type JSAppName = String
object JSAppName {
  def apply(name: String): JSAppName = name
  given Show[JSAppName] = a => a
}

def log(msg: Any*)(using definedAt: DefinedAt, appName: JSAppName = JSAppName("app")): Unit =
  logDebug(msg*)

/** Logs a debug message. */
def logDebug(msg: Any*)(using definedAt: DefinedAt, appName: JSAppName = JSAppName("app")): Unit = {
  console.debug(show"[$appName] ", msg*)
  console.debug(show"[$appName]   @ $definedAt")
}

/** Logs an info message. */
def logInfo(info: Any*)(using definedAt: DefinedAt, appName: JSAppName = JSAppName("app")): Unit = {
  console.info(show"[$appName] ", info*)
  console.info(show"[$appName]   @ $definedAt")
}

/** Logs a warning message. */
def logWarning(warning: Any*)(using definedAt: DefinedAt, appName: JSAppName = JSAppName("app")): Unit = {
  console.warn(show"[$appName] ", warning*)
  console.warn(show"[$appName]   @ $definedAt")
}

/** Logs an error message. */
def logError(err: Any*)(using definedAt: DefinedAt, appName: JSAppName = JSAppName("app")): Unit = {
  console.error(show"[$appName] ", err*)
  console.error(show"[$appName]   @ $definedAt")
}

def logAt(level: LogLevel, msg: Any)(using definedAt: DefinedAt, appName: JSAppName = JSAppName("app")): Unit =
  level match {
    case LogLevel.Debug   => logDebug(msg)
    case LogLevel.Info    => logInfo(msg)
    case LogLevel.Warning => logWarning(msg)
    case LogLevel.Error   => logError(msg)
  }

enum LogLevel derives CanEqual { case Debug, Info, Warning, Error }
