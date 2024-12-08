package framework.utils

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import dev.profunktor.redis4cats
import dev.profunktor.redis4cats.connection.{RedisClient, RedisURI}
import dev.profunktor.redis4cats.data.RedisCodec
import dev.profunktor.redis4cats.pubsub.{PubSub, PubSubCommands}
import dev.profunktor.redis4cats.{Redis, RedisCommands}
import framework.prelude.{*, given}

trait ServerAppWithRedis { self: ServerApp =>
  given redis4cats.effect.Log[IO] = new redis4cats.effect.Log[IO] {
    override def debug(msg: => String): IO[Unit] = log.debug(show"[redis4cats] $msg")
    override def error(msg: => String): IO[Unit] = log.error(show"[redis4cats] $msg")
    override def info(msg: => String): IO[Unit] = log.info(show"[redis4cats] $msg")
  }

  def redisResource[K, V](redisUri: RedisURI, codec: RedisCodec[K, V])(using redis4cats.effect.Log[IO]): Resource[
    IO,
    (
      RedisCommands[IO, K, V],
      PubSubCommands[[X] =>> Stream[IO, X], K, V],
    ),
  ] = {
    for {
      _ <- Resource.eval(log.info(show"Connecting to Redis server at '$redisUri'..."))
      redisClient <- RedisClient[IO].fromUri(redisUri)
      _ <- Resource.eval(log.info(show"Connected to Redis server at '$redisUri'."))
      redisCmd <- Redis[IO].fromClient(redisClient, codec)
      redisPubSub <- PubSub.mkPubSubConnection[IO, K, V](redisClient, codec)
    } yield (redisCmd, redisPubSub)
  }

  /** Returns Redis resource using UTF-8 codec. */
  def redisResourceUtf8(redisUri: RedisURI)(using redis4cats.effect.Log[IO]): Resource[
    IO,
    (
      RedisCommands[IO, String, String],
      PubSubCommands[[X] =>> Stream[IO, X], String, String],
    ),
  ] =
    redisResource(redisUri, RedisCodec.Utf8)
}
