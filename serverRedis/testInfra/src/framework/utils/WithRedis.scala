package framework.utils

import cats.effect.resource_shared_memoized.*
import cats.effect.std.Supervisor
import cats.effect.{IO, Resource, SyncIO}
import cats.syntax.all.*
import dev.profunktor.redis4cats.pubsub.PubSubCommands
import framework.exts.*
import framework.prelude.*
import framework.redis.{RedisChannelTyped, RedisKeyPrefix}
import framework.utils.RedisInit.RedisResourceResult
import io.scalaland.chimney.PartialTransformer
import munit.*

import scala.concurrent.duration.*

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

  def redisResourceDelayedRelease: FiniteDuration = 150.millis

  /** The cached version of [[redisResource]] shared between all tests. */
  lazy val redisResourceCached: Resource[IO, RedisResourceResult[IO, K, V]] =
    ResourceSharedMemoized
      .memoizeWithDelayedRelease(
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
        ),
        redisResourceDelayedRelease,
      )
      .unsafeRunSync()(using cats.effect.unsafe.implicits.global)

  given redisPrefixGiven: RedisKeyPrefix[K] = redisPrefix
}
