package framework.utils

import cats.effect.{IO, Resource, SyncIO}
import cats.syntax.all.*
import framework.exts.*
import framework.prelude.*
import framework.redis.RedisKeyPrefix
import framework.utils.RedisInit.RedisResourceResult
import munit.*

/** Gives you access to Redis. */
trait RedisFixture[K, V] { self: CatsEffectSuite & FrameworkTestSuiteHelpers =>

  /** The config of Redis for the tests. */
  def redisResource: Resource[IO, RedisResourceResult[IO, K, V]]

  /** The prefix to use in tests. */
  def redisPrefix: RedisKeyPrefix[K]

  given RedisKeyPrefix[K] = redisPrefix

  /** Redis fixture. */
  def redis: SyncIO[FunFixture[RedisResourceResult[IO, K, V]]] =
    ResourceFunFixture(redisResource)
}
