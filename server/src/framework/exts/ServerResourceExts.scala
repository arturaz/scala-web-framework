package framework.exts

import scribe.Scribe
import scribe.Level
import scribe.mdc.MDC
import sourcecode.*
import cats.syntax.all.*
import cats.FlatMap

extension [F[_], A](resource: Resource[F, A]) {

  /** Logs the resource using the given `log` and `logLevel`. */
  def logged(name: String, log: Scribe[F], logLevel: Level = Level.Info)(using
    Pkg,
    FileName,
    Name,
    Line,
    FlatMap[F],
  ): Resource[F, A] = for {
    _ <- Resource
      .make(log.log(logLevel, MDC.instance, show"Acquiring: $name"))(_ =>
        log.log(logLevel, MDC.instance, show"Releasing: $name.")
      )
    a <- resource
  } yield a
}
