package dev.profunktor.redis4cats.otel4s.utils

import cats.ApplicativeThrow
import cats.syntax.all.*
import dev.profunktor.redis4cats.otel4s.CommandWrapper
import io.lettuce.core.RedisCommandTimeoutException
import org.typelevel.otel4s.Attribute

/** Adds the command name and attributes to the [[RedisCommandTimeoutException]] message. */
object EnhanceTimeoutException {
  def apply[F[_]: ApplicativeThrow]: CommandWrapper[F] = new CommandWrapper[F] {
    override def wrap[A](name: String, attributes: collection.immutable.Iterable[Attribute[?]] = Nil)(fa: F[A]) =
      fa.recoverWith { case ex: RedisCommandTimeoutException =>
        ApplicativeThrow[F].raiseError(
          new RedisCommandTimeoutException(
            // Unfortunately `RedisCommandTimeoutException` does not have the `(message, cause)` constructor :(
            new Exception(
              s"Command '$name' timed out (attributes=${attributes.mkString("[", ", ", "]")}): ${ex.getMessage}",
              ex
            )
          )
        )
      }
  }
}
