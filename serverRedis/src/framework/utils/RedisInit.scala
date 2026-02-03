package framework.utils

import cats.effect.kernel.{Async, Clock, Resource}
import cats.effect.{IO, MonadCancelThrow}
import cats.syntax.all.*
import dev.profunktor.redis4cats
import dev.profunktor.redis4cats.connection.{RedisClient, RedisURI}
import dev.profunktor.redis4cats.data.RedisCodec
import dev.profunktor.redis4cats.effect.MkRedis
import dev.profunktor.redis4cats.otel4s.utils.EnhanceTimeoutException
import dev.profunktor.redis4cats.otel4s.{
  withWrapper,
  CommandWrapper,
  TracedPubSubCommands,
  TracedRedisCommands,
  TracedRedisConfig,
  TracedStreaming,
}
import dev.profunktor.redis4cats.pubsub.{PubSub, PubSubCommands}
import dev.profunktor.redis4cats.streams.{RedisStream, Streaming}
import dev.profunktor.redis4cats.{Redis, RedisCommands}
import framework.prelude.{*, given}
import io.lettuce.core.TimeoutOptions.TimeoutSource
import io.lettuce.core.protocol.RedisCommand
import io.lettuce.core.{ClientOptions, TimeoutOptions}
import org.typelevel.otel4s.trace.TracerProvider
import scribe.Scribe

/** Helpers for Redis initialization. */
object RedisInit {

  /** Default logger for Redis4Cats. */
  given defaultLogger: redis4cats.effect.Log[IO] = loggerFor(log)

  /** Converts a [[Scribe]] logger to a [[redis4cats.effect.Log]] instance. */
  def loggerFor[F[_]](log: Scribe[F], prefix: String = "[redis4cats] "): redis4cats.effect.Log[F] =
    new redis4cats.effect.Log[F] {
      override def debug(msg: => String): F[Unit] = log.debug(show"$prefix$msg")
      override def error(msg: => String): F[Unit] = log.error(show"$prefix$msg")
      override def info(msg: => String): F[Unit] = log.info(show"$prefix$msg")
    }

  /** @param client
    *   the Redis client with an established connection
    * @param commandWrapper
    *   the command wrapper used in the resource
    * @param mkCommands
    *   the resource for the commands
    * @param mkPubSub
    *   the resource for the pubsub
    * @param mkStreaming
    *   the resource for the streaming
    */
  case class RedisResourceResult[F[_], K, V](
    client: RedisClient,
    commandWrapper: CommandWrapper[F],
    mkCommands: Resource[F, RedisCommands[F, K, V]],
    mkPubSub: Resource[F, TracedPubSubCommands[F, [X] =>> Stream[F, X], K, V]],
    mkStreaming: Resource[F, TracedStreaming[F, [X] =>> Stream[F, X], K, V]],
  )

  /** Returns Redis resource with tracing and logging. */
  def redisResource[F[_]: Async, K, V](
    redisUri: RedisURI,
    codec: RedisCodec[K, V],
    tracingConfig: TracedRedisConfig[F, K, V],
    logCommandsAt: Option[(Scribe[F], scribe.Level)] = Some((framework.prelude.log, scribe.Level.Debug)),
  )(using
    log: redis4cats.effect.Log[F],
    tracerProvider: TracerProvider[F],
  ): Resource[F, RedisResourceResult[F, K, V]] = {
    for {
      _ <- Resource.eval(log.info(show"Connecting to Redis server at '$redisUri'..."))
      options = ClientOptions.builder().build()
      redisClient <- RedisClient[F].custom(redisUri, options)
      _ <- Resource.eval(log.info(show"Connected to Redis server at '$redisUri'."))
      exceptionWrapper = EnhanceTimeoutException[F]
      loggingWrapper = logCommandsAt.fold(CommandWrapper.noOp) { case (log, level) =>
        LoggingCommandWrapper(log, level)
      }
      commandWrapper = loggingWrapper.combine(exceptionWrapper)
      mkCmd = Redis[F]
        .fromClient(redisClient, codec)
        .evalMap(TracedRedisCommands(_, tracingConfig).map(cmd => cmd.withWrapper[F](w => w.combine(commandWrapper))))
      mkPubSub = PubSub
        .mkPubSubConnection[F, K, V](redisClient, codec)
        .evalMap(
          TracedPubSubCommands(_, tracingConfig)
            .map(cmd => cmd.withWrapper[F](w => w.combine(commandWrapper)))
        )
      mkStreaming = RedisStream
        .mkStreamingConnectionResource[F, K, V](redisClient, codec)
        .evalMap(
          TracedStreaming(_, tracingConfig)
            .map(cmd => cmd.withWrapper[F](w => w.combine(commandWrapper)))
        )
    } yield RedisResourceResult(redisClient, commandWrapper, mkCmd, mkPubSub, mkStreaming)
  }

  /** Returns Redis resource using a string based (UTF-8 by default) codec. */
  def redisResourceStringCodec[F[_]: Async](
    redisUri: RedisURI,
    tracingConfig: TracedRedisConfig[F, String, String],
    codec: RedisCodec[String, String] = RedisCodec.Utf8,
    logCommandsAt: Option[(Scribe[F], scribe.Level)] = Some((framework.prelude.log, scribe.Level.Debug)),
  )(using
    log: redis4cats.effect.Log[F],
    tracerProvider: TracerProvider[F],
  ): Resource[F, RedisResourceResult[F, String, String]] =
    redisResource(redisUri, codec, tracingConfig, logCommandsAt)
}
