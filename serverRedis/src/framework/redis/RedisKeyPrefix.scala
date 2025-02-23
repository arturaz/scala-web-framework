package framework.redis

import cats.Show
import cats.kernel.Semigroup
import cats.syntax.semigroup.*
import dev.profunktor.redis4cats.data.{RedisChannel, RedisPattern}
import org.tpolecat.typename.TypeName

import scala.reflect.ClassTag

/** A prefix for the Redis keys.
  *
  * Very useful when you share the same Redis instance between multiple applications or between development and test
  * environments.
  */
final case class RedisKeyPrefix[+Key](prefix: Key) {

  /** Creates a composite key with this prefix and the specified key. */
  def key[K2 >: Key](key: K2)(using Semigroup[K2]): RedisKey[K2] = RedisKey(this, key)

  /** Creates a channel with the specified key. */
  def channel[K2 >: Key](key: K2)(using Semigroup[K2]): RedisChannel[K2] =
    RedisChannel((prefix: K2) |+| key)

  /** Creates a typed channel with the specified key. */
  def channelOf[Data]: RedisKeyPrefix.ChannelOfBuilder[Key, Data] =
    RedisKeyPrefix.ChannelOfBuilder(this)

  /** Creates a pattern with the specified key. */
  def pattern[K2 >: Key](key: K2)(using Semigroup[K2]): RedisPattern[K2] =
    RedisPattern((prefix: K2) |+| key)

  /** Creates a typed pattern with the specified key. */
  def patternOf[Data]: RedisKeyPrefix.PatternOfBuilder[Key, Data] =
    RedisKeyPrefix.PatternOfBuilder(this)
}
object RedisKeyPrefix {
  case class ChannelOfBuilder[+Key, Data](private val prefix: RedisKeyPrefix[Key]) extends AnyVal {
    inline def apply[K2 >: Key](key: K2)(using Semigroup[K2], Show[K2], TypeName[Data]): RedisChannelTyped[K2, Data] =
      RedisChannelTyped(prefix.channel[K2](key))
  }

  case class PatternOfBuilder[+Key, Data](private val prefix: RedisKeyPrefix[Key]) extends AnyVal {
    inline def apply[K2 >: Key](key: K2)(using Semigroup[K2], Show[K2], TypeName[Data]): RedisPatternTyped[K2, Data] =
      RedisPatternTyped(prefix.pattern[K2](key))
  }
}
