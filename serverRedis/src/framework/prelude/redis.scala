package framework.prelude

import cats.{Applicative, Traverse}
import dev.profunktor.redis4cats.connection.RedisURI
import dev.profunktor.redis4cats.data.RedisPatternEvent

given Show[RedisURI] = _.underlying.toString()

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
