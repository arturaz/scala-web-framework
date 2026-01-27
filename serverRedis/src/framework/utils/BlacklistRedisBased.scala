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
  case class ExtractFromClaimResult[+Id](id: Id, claim: JwtClaim)

  /** Blacklists JWT claims.
    *
    * @param extractFromClaim
    *   A function that extracts data from the JWT claim. If [[None]] is returned, no blacklisting is done.
    */
  def forJWT[F[_]: Monad: Clock, BlacklistedValue, RKey: Semigroup, RValue: Empty](
    cmd: RedisCommands[F, RKey, RValue],
    key: RedisKeyPrefix[RKey],
    extractFromClaim: BlacklistedValue => Option[ExtractFromClaimResult[RKey]],
  ): Blacklist[F, BlacklistedValue] = new {
    def keyFor(a: BlacklistedValue): Option[RedisKey[RKey]] =
      extractFromClaim(a).map(v => key.key(v.id))

    override def isBlacklisted(a: BlacklistedValue): F[Boolean] =
      keyFor(a).fold(false.pure)(cmd.exists(_))

    override def blacklist(a: BlacklistedValue): F[Unit] = {
      extractFromClaim(a) match {
        case None => Monad[F].unit
        case Some(result) =>
          for {
            ttl <- result.claim.expiration match {
              case Some(secondsSinceEpoch) =>
                val expiresAt = FrameworkDateTime.fromUnixSeconds(secondsSinceEpoch)
                Clock[F].realTimeInstant.map(now => Some(expiresAt - FrameworkDateTime.fromInstant(now)))

              case None => None.pure
            }
            setArgs = SetArgs(existence = None, ttl = ttl.map(Ttl.Px(_)))
            _ <- cmd.set(key.key(result.id), Empty[RValue].empty, setArgs)
          } yield ()
      }
    }
  }
}
