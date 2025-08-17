package framework.redis

import cats.kernel.Semigroup
import cats.syntax.semigroup.*
import cats.syntax.show.*
import cats.Show

/** A prefixed Redis key. */
final case class RedisKey[+Key: Semigroup](prefix: RedisKeyPrefix[Key], key: Key) {
  val fullKey: Key = prefix.prefix |+| key

  override def toString: String = s"RedisKey($fullKey)"
}
object RedisKey {
  given [Key: Show]: Show[RedisKey[Key]] = _.fullKey.show
  given [Key]: Conversion[RedisKey[Key], Key] = _.fullKey
  given [Key: Show]: Conversion[RedisKey[Key], String] = _.fullKey.show
}
