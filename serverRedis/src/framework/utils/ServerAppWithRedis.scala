package framework.utils

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import dev.profunktor.redis4cats
import dev.profunktor.redis4cats.connection.{RedisClient, RedisURI}
import dev.profunktor.redis4cats.data.RedisCodec
import dev.profunktor.redis4cats.otel4s.{TracedPubSubCommands, TracedRedisCommands, TracedRedisConfig, TracedStreaming}
import dev.profunktor.redis4cats.pubsub.{PubSub, PubSubCommands}
import dev.profunktor.redis4cats.streams.{RedisStream, Streaming}
import dev.profunktor.redis4cats.{Redis, RedisCommands}
import framework.config.IsProductionMode
import framework.prelude.{*, given}
import org.typelevel.otel4s.trace.TracerProvider
import dev.profunktor.redis4cats.otel4s.utils.EnhanceTimeoutException

case class RedisResourceResult[K, V](
  client: RedisClient,
  commands: RedisCommands[IO, K, V],
  mkPubSub: Resource[IO, TracedPubSubCommands[IO, [X] =>> Stream[IO, X], K, V]],
  mkStreaming: Resource[IO, TracedStreaming[IO, [X] =>> Stream[IO, X], K, V]],
)

trait ServerAppWithRedis { self: ServerApp =>
  given redis4cats.effect.Log[IO] = new redis4cats.effect.Log[IO] {
    override def debug(msg: => String): IO[Unit] = log.debug(show"[redis4cats] $msg")
    override def error(msg: => String): IO[Unit] = log.error(show"[redis4cats] $msg")
    override def info(msg: => String): IO[Unit] = log.info(show"[redis4cats] $msg")
  }

  def redisResource[K, V](
    redisUri: RedisURI,
    codec: RedisCodec[K, V],
    tracingConfig: TracedRedisConfig[IO, K, V],
  )(using
    log: redis4cats.effect.Log[IO],
    tracerProvider: TracerProvider[IO],
  ): Resource[IO, RedisResourceResult[K, V]] = {
    for {
      _ <- Resource.eval(log.info(show"Connecting to Redis server at '$redisUri'..."))
      redisClient <- RedisClient[IO].fromUri(redisUri)
      _ <- Resource.eval(log.info(show"Connected to Redis server at '$redisUri'."))
      redisCmd <- Redis[IO].fromClient(redisClient, codec)
      exceptionWrapper = EnhanceTimeoutException[IO]
      redisCmd <- TracedRedisCommands(redisCmd, tracingConfig)
        .map(cmd => cmd.withWrapper(w => w.combine(exceptionWrapper)))
        .toResource
      mkPubSub = PubSub
        .mkPubSubConnection[IO, K, V](redisClient, codec)
        .evalMap(
          TracedPubSubCommands(_, tracingConfig)
            .map(cmd => cmd.withWrapper(w => w.combine(exceptionWrapper)))
        )
      mkStreaming = RedisStream
        .mkStreamingConnectionResource[IO, K, V](redisClient, codec)
        .evalMap(
          TracedStreaming(_, tracingConfig)
            .map(cmd => cmd.withWrapper(w => w.combine(exceptionWrapper)))
        )
    } yield RedisResourceResult(redisClient, redisCmd, mkPubSub, mkStreaming)
  }

  /** Returns Redis resource using UTF-8 codec. */
  def redisResourceUtf8(redisUri: RedisURI, tracingConfig: TracedRedisConfig[IO, String, String])(using
    redis4cats.effect.Log[IO],
    TracerProvider[IO],
  ): Resource[IO, RedisResourceResult[String, String]] =
    redisResource(redisUri, RedisCodec.Utf8, tracingConfig)
}
