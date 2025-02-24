package framework.utils

import cats.effect.resource_shared_memoized.*
import cats.effect.{IO, Resource, SyncIO}
import cats.syntax.all.*
import framework.exts.*
import framework.prelude.*
import framework.redis.RedisKeyPrefix
import framework.utils.RedisInit.RedisResourceResult
import munit.*

/** Mix this into an object to get access to Redis.
  *
  * @param redisResource
  *   The config of Redis for the tests.
  * @param redisPrefix
  *   The prefix to use in tests.
  */
trait WithRedis[K, V](
  val redisResource: Resource[IO, RedisResourceResult[IO, K, V]],
  val redisPrefix: RedisKeyPrefix[K],
) {

  /** The cached version of [[redisResource]] shared between all tests. */
  lazy val redisResourceCached: Resource[IO, RedisResourceResult[IO, K, V]] =
    ResourceSharedMemoized
      .memoize(
        for {
          r <- redisResource
          // Cache commands and pubsub as they would be shared in real applicaiton, where as streaming would not.
          mkCommands <- r.mkCommands.memoizeShared.toResource
          mkPubSub <- r.mkPubSub.memoizeShared.toResource
        } yield RedisResourceResult(
          r.client,
          r.commandWrapper,
          mkCommands,
          mkPubSub,
          r.mkStreaming,
        )
      )
      .unsafeRunSync()(using cats.effect.unsafe.implicits.global)

  given redisPrefixGiven: RedisKeyPrefix[K] = redisPrefix
}
