package framework.redis

import cats.syntax.semigroup.*
import cats.syntax.show.*
import cats.{Semigroup, Show}

/** A prefixed Redis key. */
final case class RedisKey[+Key: Semigroup](prefix: RedisKeyPrefix[Key], key: Key) {
  val fullKey: Key = prefix.prefix |+| key

  override def toString: String = s"RedisKey($fullKey)"
}
object RedisKey {
  given [Key: Show]: Show[RedisKey[Key]] = _.fullKey.show
  given [Key]: Conversion[RedisKey[Key], Key] = _.fullKey
  given [Key: Show]: Conversion[RedisKey[Key], String] = _.fullKey.show

  extension [Key](key: RedisKey[Key]) {
    def mapPrefix(f: Key => Key)(using Semigroup[Key]): RedisKey[Key] = key.copy(prefix = key.prefix.map(f))

    def map(f: Key => Key)(using Semigroup[Key]): RedisKey[Key] = key.copy(key = f(key.key))
  }
}
