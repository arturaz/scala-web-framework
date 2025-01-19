package framework.redis

import cats.kernel.Semigroup
import cats.syntax.semigroup.*
import cats.Show

/** A prefixed Redis key. */
final case class RedisKey[+Key: Semigroup](prefix: RedisAppKeyPrefix[Key], key: Key) {
  val fullKey: Key = prefix.prefix |+| key

  override def toString: String = s"RedisKey($fullKey)"
}
object RedisKey {
  given [Key]: Show[RedisKey[Key]] = _.toString
  given Conversion[RedisKey[?], String] = _.toString
}
