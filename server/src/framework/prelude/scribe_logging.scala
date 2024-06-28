package framework.prelude

import scribe.cats.*
import scribe.{Logger, Scribe}

val logGlobal: Scribe[IO] = scribe.cats[IO]
val appLogger: Logger = scribe.Logger("app")
val log: Scribe[IO] = appLogger.f[IO]
