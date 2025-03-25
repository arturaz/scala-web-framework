package framework.utils

import cats.effect.MonadCancelThrow
import cats.effect.kernel.{Clock, Outcome}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import dev.profunktor.redis4cats.otel4s.CommandWrapper
import org.typelevel.otel4s.Attribute
import scribe.{Level, Scribe}
import framework.exts.*

/** Logs every executed Redis command at the given level. */
def LoggingCommandWrapper[F[_]: MonadCancelThrow: Clock](log: Scribe[F], level: Level): CommandWrapper[F] = {
  given PrettyPrintDuration.Strings = PrettyPrintDuration.Strings.EnglishShortNoSpaces

  new {
    override def wrap[A](name: String, attributes: collection.immutable.Iterable[Attribute[?]])(fa: F[A]): F[A] = {
      inline def commandStr = {
        val attrs =
          attributes.iterator.map(a => show"${a.key.name}=${a.value.toString()}").mkString("[\n", ",\n", "]\n")
        show"Redis command '$name' with attributes $attrs"
      }

      for {
        startAt <- Clock[F].monotonic
        _ <- log.log(level, summon, show"Starting $commandStr")
        a <- fa.guaranteeCase { outcome =>
          for {
            endAt <- Clock[F].monotonic
            duration = endAt - startAt
            durationStr = duration.prettyUnbounded
            _ <- outcome match {
              case Outcome.Succeeded(fa) => log.log(level, summon, show"$commandStr succeeded in $durationStr")
              case Outcome.Errored(e) =>
                log.log(level, summon, show"$commandStr errored in $durationStr: ${e.toString()}")
              case Outcome.Canceled() =>
                log.log(level, summon, show"$commandStr was canceled after $durationStr")
            }
          } yield ()

        }
      } yield a
    }
  }
}
