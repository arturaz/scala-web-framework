package framework.prelude

import framework.sourcecode.DefinedAt

def log(msg: Any)(using definedAt: DefinedAt): Unit = {
  println(
    s"""[INFO] $msg
       |[INFO]   @ $definedAt""".stripMargin
  )
}

/** Logs a warning message. */
def logWarning(warning: Any)(using definedAt: DefinedAt): Unit = {
  println(
    s"""[WARN] $warning
       |[WARN]   @ $definedAt""".stripMargin
  )
}

/** Logs an error message. */
def logError(err: Any)(using definedAt: DefinedAt): Unit = {
  Console.err.println(
    s"""[ERROR] $err
       |[ERROR]   @ $definedAt""".stripMargin
  )
}

def logAt(level: LogLevel, msg: Any)(using definedAt: DefinedAt): Unit =
  level match
    case LogLevel.Info    => log(msg)
    case LogLevel.Warning => logWarning(msg)
    case LogLevel.Error   => logError(msg)

enum LogLevel derives CanEqual { case Info, Warning, Error }
