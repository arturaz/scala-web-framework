package framework.utils

import alleycats.Empty
import cats.Monad
import cats.effect.kernel.Clock
import cats.kernel.Semigroup
import cats.syntax.all.*
import dev.profunktor.redis4cats.RedisCommands
import dev.profunktor.redis4cats.effects.SetArg.Ttl
import dev.profunktor.redis4cats.effects.SetArgs
import framework.data.FrameworkDateTime
import framework.redis.{RedisKey, RedisKeyPrefix}
import pdi.jwt.JwtClaim

import java.time.LocalDateTime
import scala.concurrent.duration.FiniteDuration

/** Redis-based JWT blacklist. */
object JWTBlacklistRedisBased {
  def apply[F[_]: Monad: Clock, Key: Semigroup, Value: Empty](
    key: RedisKeyPrefix[Key],
    stringToKey: String => Key,
    cmd: RedisCommands[F, Key, Value],
  ): JWTBlacklist[F] = new {
    def keyFor(jwt: JwtClaim): RedisKey[Key] = key.key(
      stringToKey(
        // Use the ID if available, otherwise fallback to the content
        jwt.jwtId.getOrElse(jwt.content)
      )
    )

    override def isBlacklisted(jwt: JwtClaim): F[Boolean] =
      cmd.exists(keyFor(jwt))

    override def blacklist(jwt: JwtClaim): F[Unit] = {
      for {
        ttl <- jwt.expiration match {
          case Some(secondsSinceEpoch) =>
            val expiresAt = FrameworkDateTime.fromUnixSeconds(secondsSinceEpoch)
            Clock[F].realTimeInstant.map(now => Some(expiresAt - FrameworkDateTime.fromInstant(now)))

          case None => None.pure
        }
        setArgs = SetArgs(existence = None, ttl = ttl.map(Ttl.Px(_)))
        _ <- cmd.set(keyFor(jwt), Empty[Value].empty, setArgs)
      } yield ()
    }
  }
}
