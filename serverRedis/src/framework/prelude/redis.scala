package framework.prelude

import cats.{Applicative, Traverse}
import dev.profunktor.redis4cats.connection.RedisURI
import dev.profunktor.redis4cats.data.RedisPatternEvent
import dev.profunktor.redis4cats.data.RedisChannel
import dev.profunktor.redis4cats.data.RedisPattern
import dev.profunktor.redis4cats.pubsub.data.Subscription

given Show[RedisURI] = _.underlying.toString()

given [K: CirceKeyDecoder]: CirceKeyDecoder[RedisChannel[K]] = CirceKeyDecoder[K].map(RedisChannel(_))
given [K: CirceKeyEncoder]: CirceKeyEncoder[RedisChannel[K]] = CirceKeyEncoder[K].contramap(_.underlying)
given [K: CirceEncoder]: CirceEncoder[RedisChannel[K]] = CirceEncoder[K].contramap(_.underlying)
given [K: CirceDecoder]: CirceDecoder[RedisChannel[K]] = CirceDecoder[K].map(RedisChannel(_))

given [K: CirceKeyDecoder]: CirceKeyDecoder[RedisPattern[K]] = CirceKeyDecoder[K].map(RedisPattern(_))
given [K: CirceKeyEncoder]: CirceKeyEncoder[RedisPattern[K]] = CirceKeyEncoder[K].contramap(_.underlying)
given [K: CirceEncoder]: CirceEncoder[RedisPattern[K]] = CirceEncoder[K].contramap(_.underlying)
given [K: CirceDecoder]: CirceDecoder[RedisPattern[K]] = CirceDecoder[K].map(RedisPattern(_))

given [K: CirceEncoder]: CirceEncoder[Subscription[K]] = CirceEncoder.derived
given [K: CirceDecoder]: CirceDecoder[Subscription[K]] = CirceDecoder.derived

given [K]: Traverse[[V] =>> RedisPatternEvent[K, V]] with {
  // Members declared in cats.Foldable
  def foldLeft[A, B](fa: RedisPatternEvent[K, A], b: B)(f: (B, A) => B): B =
    f(b, fa.data)

  def foldRight[A, B](fa: RedisPatternEvent[K, A], lb: cats.Eval[B])(
    f: (A, cats.Eval[B]) => cats.Eval[B]
  ): cats.Eval[B] =
    f(fa.data, lb)

  // Members declared in cats.Traverse
  def traverse[G[_]: Applicative, A, B](fa: RedisPatternEvent[K, A])(f: A => G[B]): G[RedisPatternEvent[K, B]] =
    f(fa.data).map(b => RedisPatternEvent(fa.pattern, fa.channel, b))
}
