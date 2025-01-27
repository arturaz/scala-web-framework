package framework.exts

import cats.effect.Concurrent
import cats.syntax.all.*
import cats.{Applicative, Foldable}
import dev.profunktor.redis4cats.data.{RedisChannel, RedisPatternEvent}
import dev.profunktor.redis4cats.pubsub.PubSubCommands
import dev.profunktor.redis4cats.streams.Streaming
import dev.profunktor.redis4cats.streams.data.{MessageId, XAddMessage}
import framework.data.MapEncoder
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

  /** Publishes a message to all specified channels once. */
  def publishOnceToAll[C[_]: Foldable](channels: C[RedisChannel[K]], value: V)(using Concurrent[F]): F[Unit] =
    channels.traverse_(publishOnce(_, value))

  /** Publishes a message to all specified channels once. */
  def publishOnceToAllTyped[C[_]: Foldable, M](channels: C[RedisChannelTyped[K, M]], value: M)(using
    Transformer[M, V],
    Concurrent[F],
  ): F[Unit] =
    channels.traverse_(publishOnceTyped(_, value))

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

  /** Subscribes to the specified pattern and transforms the value using the [[PartialTransformer]].
    *
    * If the transformation fails, `onTransformError` will be called with the original value and the message will be
    * ignored.
    *
    * @see
    *   [[PubSubCommands.psubscribe]]
    */
  def psubscribeTyped[M](
    pattern: RedisPatternTyped[K, M]
  )(using
    transformer: PartialTransformer[V, M],
    applicative: Applicative[F],
    onTransformError: OnRedisMessageTransformError[F, Unit, RedisPatternEvent[K, V]],
  ): Stream[F, RedisPatternEvent[K, M]] = {
    pubSub
      .psubscribe(pattern.pattern)
      .evalMapChunk { evt =>
        transformer.transform(evt.data) match {
          case Result.Value(value) =>
            Right(evt.copy(data = value: M /* Without the type ascription the stream just silently fails :/ */ )).pure
          case errors: Result.Errors => onTransformError.onTransformError(RedisChannel(()), evt, errors).as(Left(()))
        }
      }
      .collect { case Right(value) => value }
  }
}

extension [F[_], K, V](streaming: Streaming[[X] =>> Stream[F, X], K, V]) {

  /** Appends a single message to the stream. */
  def appendOnce(message: XAddMessage[K, V])(using Concurrent[F]): F[MessageId] = {
    val pipe = streaming.append
    Stream.emit(message).through(pipe).compile.onlyOrError
  }
}

extension (add: XAddMessage.type) {
  def typed[Body, K, V](
    key: K,
    body: Body,
    approxMaxlen: Option[Long] = None,
    minId: Option[String] = None,
  )(using encoder: MapEncoder[K, V, Body]) =
    add(key = key, body = encoder.encode(body), approxMaxlen = approxMaxlen, minId = minId)
}

extension (id: MessageId) {
  def toTyped: Either[String, MessageIdTyped] = MessageIdTyped.from(id)

  def toTypedOrThrow: MessageIdTyped = toTyped.getOrThrow
}
