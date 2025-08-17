package framework.utils

trait BlacklistChecker[F[_], -A] {

  /** Returns true if the value is blacklisted. */
  def isBlacklisted(a: A): F[Boolean]
}

trait Blacklister[F[_], -A] {

  /** Blacklists the value. */
  def blacklist(a: A): F[Unit]
}

trait Blacklist[F[_], -A] extends BlacklistChecker[F, A], Blacklister[F, A]
