package framework.prelude

import framework.config.IsProductionMode
import scribe.*
import scribe.cats.*
import scribe.file.{FileWriter, FlushMode, PathBuilder}

import java.nio.charset.StandardCharsets
import java.nio.file.Paths

// Must be a `def` to make it get the latest applied logger each time.
def logGlobal: Scribe[IO] = scribe.cats[IO]

// Must be a `def` to make it get the latest applied logger each time.
def appLogger: Logger = Logger("app")

// Must be a `def` to make it get the latest applied logger each time.
def log: Scribe[IO] = appLogger.f[IO]

def applyLoggingDefaults(isProduction: IsProductionMode): SyncIO[Unit] = SyncIO {
  if (!isProduction) {
    appLogger.info("Applying development logging defaults.")

    // Also log to a file
    val logPath = Paths.get("development.log")
    appLogger.info(s"Additionally logging to \"$logPath\".")

    val _ = Logger.root
      .withHandler(
        writer = FileWriter(PathBuilder.static(logPath), append = false, FlushMode.AlwaysFlush, StandardCharsets.UTF_8)
      )
      // .withMinimumLevel(Level.Debug)
      .replace()

    val _ = Logger("doobie").withMinimumLevel(Level.Debug).replace()
    // val _ = Logger("io.lettuce.core.protocol.CommandHandler").withMinimumLevel(Level.Trace).replace()
//    val _ = Logger("org.flywaydb").withMinimumLevel(Level.Trace).replace()

    // Log at debug level for application code.
    val _ = appLogger.withMinimumLevel(Level.Debug).replace()

    appLogger.info("Development logging defaults applied.")
  }
}
