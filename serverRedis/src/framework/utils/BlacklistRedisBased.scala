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

/** Redis-based blacklist. */
object BlacklistRedisBased {

  /** Blacklists JWT claims.
    *
    * @param extractIdFromClaim
    *   A function that extracts an ID from the JWT claim. If [[None]] is returned, JWT id is used (if available),
    *   otherwise the JWT content is used.
    */
  def forJWT[F[_]: Monad: Clock, BlacklistedValue, RKey: Semigroup, RValue: Empty](
    cmd: RedisCommands[F, RKey, RValue],
    key: RedisKeyPrefix[RKey],
    extractIdFromClaim: BlacklistedValue => RKey,
    extractClaimFromValue: BlacklistedValue => JwtClaim,
  ): Blacklist[F, BlacklistedValue] = new {
    def keyFor(a: BlacklistedValue): RedisKey[RKey] =
      key.key(extractIdFromClaim(a))

    override def isBlacklisted(a: BlacklistedValue): F[Boolean] =
      cmd.exists(keyFor(a))

    override def blacklist(a: BlacklistedValue): F[Unit] = {
      for {
        jwt = extractClaimFromValue(a)
        ttl <- jwt.expiration match {
          case Some(secondsSinceEpoch) =>
            val expiresAt = FrameworkDateTime.fromUnixSeconds(secondsSinceEpoch)
            Clock[F].realTimeInstant.map(now => Some(expiresAt - FrameworkDateTime.fromInstant(now)))

          case None => None.pure
        }
        setArgs = SetArgs(existence = None, ttl = ttl.map(Ttl.Px(_)))
        _ <- cmd.set(keyFor(a), Empty[RValue].empty, setArgs)
      } yield ()
    }
  }
}
