package framework.utils

import scala.concurrent.duration.*
import pdi.jwt.JwtClaim

trait JWTBlacklistChecker[F[_]] {

  /** Returns true if the JWT is blacklisted. */
  def isBlacklisted(jwt: JwtClaim): F[Boolean]
}

trait JWTBlacklister[F[_]] {

  /** Blacklists the JWT until it expires, or if [[JwtClaim.expiration]] is not set, indefinitely. */
  def blacklistFor(jwt: JwtClaim): F[Unit]
}

trait JWTBlacklist[F[_]] extends JWTBlacklistChecker[F] with JWTBlacklister[F]
