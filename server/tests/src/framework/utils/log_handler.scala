package framework.utils

import cats.effect.kernel.Sync
import doobie.util.log.LogHandler

given doobieLogHandler[M[_]: Sync]: LogHandler[M] = LogHandler.jdkLogHandler
