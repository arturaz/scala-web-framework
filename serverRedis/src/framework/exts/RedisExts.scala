package framework.exts

import cats.Applicative
import cats.effect.Concurrent
import cats.syntax.all.*
import dev.profunktor.redis4cats.data.RedisChannel
import dev.profunktor.redis4cats.pubsub.PubSubCommands
import framework.redis.*
import fs2.Stream
import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.{PartialTransformer, Transformer}

extension [F[_], K, V](pubSub: PubSubCommands[[X] =>> Stream[F, X], K, V]) {

  /** Publishes a message to the specified channel once.
    *
    * @see
    *   [[PubSubCommands.publish]]
    */
  def publishOnce(channel: RedisChannel[K], value: V)(using Concurrent[F]): F[Unit] = {
    val pipe = pubSub.publish(channel)
    Stream.emit(value).through(pipe).compile.drain
  }

  /** Publishes a message to the specified channel once.
    *
    * @see
    *   [[PubSubCommands.publish]]
    */
  def publishOnceTyped[M](channel: RedisChannelTyped[K, M], value: M)(using
    transformer: Transformer[M, V],
    concurrent: Concurrent[F],
  ): F[Unit] =
    publishOnce(channel.channel, transformer.transform(value))

  /** Subscribes to the specified channel and transforms the value using the [[PartialTransformer]].
    *
    * If the transformation fails, `onTransformError` will be called with the original value and the message will be
    * ignored.
    *
    * @see
    *   [[PubSubCommands.subscribe]]
    */
  def subscribeTyped[M](
    channel: RedisChannelTyped[K, M]
  )(using
    transformer: PartialTransformer[V, M],
    applicative: Applicative[F],
    onTransformError: OnRedisMessageTransformError[F, K, V],
  ): Stream[F, M] = {
    pubSub
      .subscribe(channel.channel)
      .evalMapChunk { value =>
        transformer.transform(value) match {
          case Result.Value(value)   => Right(value).pure
          case errors: Result.Errors => onTransformError.onTransformError(channel, value, errors).as(Left(()))
        }
      }
      .collect { case Right(value) => value }
  }
}
