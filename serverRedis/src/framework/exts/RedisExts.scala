package framework.exts

import cats.effect.{Concurrent, MonadCancelThrow}
import cats.syntax.all.*
import cats.{Applicative, Foldable, Parallel, UnorderedTraverse}
import dev.profunktor.redis4cats.data.{RedisChannel, RedisCodec, RedisPatternEvent}
import dev.profunktor.redis4cats.otel4s.{TracedPubSubCommands, TracedSubscribeCommands}
import dev.profunktor.redis4cats.pubsub.{PublishCommands, SubscribeCommands}
import dev.profunktor.redis4cats.streams.Streaming
import dev.profunktor.redis4cats.streams.data.{MessageId, XAddMessage}
import framework.data.{MapEncoder, SpanOpsWithTracingData, WithTracingData}
import framework.prelude.{*, given}
import framework.redis.*
import fs2.Stream
import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.{PartialTransformer, Transformer}
import org.typelevel.otel4s.trace.{SpanOps, Tracer}

import java.nio.ByteBuffer

extension [K, V](codec: RedisCodec[K, V]) {

  /** Invariant mapping. */
  def imap[K1, V1](
    kMapper: K => K1,
    vMapper: V => V1,
  )(kContramapper: K1 => K, vContramapper: V1 => V): RedisCodec[K1, V1] = RedisCodec(
    new io.lettuce.core.codec.RedisCodec[K1, V1] {
      override def encodeValue(value: V1): ByteBuffer = codec.underlying.encodeValue(vContramapper(value))
      override def encodeKey(key: K1): ByteBuffer = codec.underlying.encodeKey(kContramapper(key))
      override def decodeValue(bytes: ByteBuffer): V1 = vMapper(codec.underlying.decodeValue(bytes))
      override def decodeKey(bytes: ByteBuffer): K1 = kMapper(codec.underlying.decodeKey(bytes))
    }
  )
}

extension [F[_], Key, Value](pubSub: PublishCommands[F, [X] =>> Stream[F, X], Key, Value]) {

  /** Publishes a message to the specified channel once.
    *
    * @see
    *   [[PubSubCommands.publish]]
    */
  def publishTyped[Message](channel: RedisChannelTyped[Key, Message], value: Message)(using
    transformer: Transformer[Message, Value],
    concurrent: Concurrent[F],
  ): F[Long] =
    pubSub.publish(channel.channel, transformer.transform(value))

  /** Sequentially publishes messages to the specified channel. */
  def publishManyTyped[Collection[_]: Foldable, Message](
    channel: RedisChannelTyped[Key, Message],
    values: Collection[Message],
  )(using
    transformer: Transformer[Message, Value],
    concurrent: Concurrent[F],
  ): F[Unit] =
    values.traverse_(pubSub.publishTyped(channel, _))

  /** Publishes a message to all specified channels once. */
  def publishToAll[Collection[_]: Foldable](channels: Collection[RedisChannel[Key]], value: Value)(using
    Concurrent[F]
  ): F[Unit] =
    channels.traverse_(pubSub.publish(_, value))

  /** Publishes a message to all specified channels once. */
  def publishToAllTyped[Collection[_]: Foldable, Message](
    channels: Collection[RedisChannelTyped[Key, Message]],
    value: Message,
  )(using
    Transformer[Message, Value],
    Concurrent[F],
  ): F[Unit] =
    channels.traverse_(publishTyped(_, value))

  /** Publishes messages to all specified channels. The messages are published in parallel to each channel, but
    * sequentially within channel.
    */
  def publishManyToAllTyped[ChannelsCollection[_]: Foldable, MessagesCollection[_]: Foldable, Message](
    channels: ChannelsCollection[RedisChannelTyped[Key, Message]],
    values: MessagesCollection[Message],
  )(using
    Transformer[Message, Value],
    Concurrent[F],
    Parallel[F],
  ): F[Unit] =
    channels.parTraverse_(publishManyTyped(_, values))
}

private inline def transformValue[F[_]: Applicative, Key, Value, Message](
  channel: RedisChannelTyped[Key, Message],
  value: Value,
)(using
  transformer: PartialTransformer[Value, Message],
  onTransformError: OnRedisMessageTransformError[F, Key, Value],
): F[Either[Unit, Message]] =
  transformer.transform(value) match {
    case Result.Value(value)   => Right(value).pure
    case errors: Result.Errors => onTransformError.onTransformError(channel, value, errors).as(Left(()))
  }

private inline def transformEvt[F[_]: Applicative, K, V, M](
  pattern: RedisPatternTyped[K, M],
  evt: RedisPatternEvent[K, V],
)(using
  transformer: PartialTransformer[V, M],
  onTransformError: OnRedisMessageTransformError[F, Unit, RedisPatternEvent[K, V]],
): F[Either[Unit, RedisPatternEvent[K, M]]] =
  transformer.transform(evt.data) match {
    case Result.Value(value) =>
      Right(evt.copy(data = value: M /* Without the type ascription the stream just silently fails :/ */ )).pure
    case errors: Result.Errors => onTransformError.onTransformError(RedisChannel(()), evt, errors).as(Left(()))
  }

extension [F[_], K, V](pubSub: SubscribeCommands[F, [X] =>> Stream[F, X], K, V]) {

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
    F: MonadCancelThrow[F],
    onTransformError: OnRedisMessageTransformError[F, K, V],
  ): Stream[F, M] = {
    pubSub.subscribe(channel.channel).evalMapChunk(transformValue(channel, _)).collect { case Right(value) => value }
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
    F: MonadCancelThrow[F],
    onTransformError: OnRedisMessageTransformError[F, Unit, RedisPatternEvent[K, V]],
  ): Stream[F, RedisPatternEvent[K, M]] = {
    pubSub.psubscribe(pattern.pattern).evalMapChunk(transformEvt(pattern, _)).collect { case Right(value) => value }
  }
}

extension [F[_], K, V](pubSub: TracedSubscribeCommands[F, [X] =>> Stream[F, X], K, V]) {

  /** Subscribes to the specified channel and transforms the value using the [[PartialTransformer]].
    *
    * If the transformation fails, `onTransformError` will be called with the original value and the message will be
    * ignored.
    *
    * @see
    *   [[PubSubCommands.subscribe]]
    */
  def subscribeTypedWithTracedEvents[M](
    channel: RedisChannelTyped[K, WithTracingData[M]]
  )(using
    transformer: PartialTransformer[V, WithTracingData[M]],
    F: MonadCancelThrow[F],
    onTransformError: OnRedisMessageTransformError[F, K, V],
  ): Stream[F, SpanOpsWithTracingData[F, M]] = {
    pubSub
      .subscribeWithTracedEvents(channel.channel)
      .evalMapChunk { case (spanOps, value) =>
        transformValue(channel, value).map(_.map(SpanOpsWithTracingData(spanOps, _)))
      }
      .collect { case Right(tpl) => tpl }
  }

  /** Subscribes to the specified pattern and transforms the value using the [[PartialTransformer]].
    *
    * If the transformation fails, `onTransformError` will be called with the original value and the message will be
    * ignored.
    *
    * @see
    *   [[PubSubCommands.psubscribe]]
    */
  def psubscribeTypedWithTracedEvents[M](
    pattern: RedisPatternTyped[K, WithTracingData[M]]
  )(using
    transformer: PartialTransformer[V, WithTracingData[M]],
    F: MonadCancelThrow[F],
    onTransformError: OnRedisMessageTransformError[F, Unit, RedisPatternEvent[K, V]],
  ): Stream[F, SpanOpsWithTracingData[F, RedisPatternEvent[K, M]]] = {
    pubSub
      .psubscribeWithTracedEvents(pattern.pattern)
      .evalMapChunk { case (spanOps, evt) =>
        transformEvt(pattern, evt).map(_.map(evt => SpanOpsWithTracingData(spanOps, evt.sequence)))
      }
      .collect { case Right(tpl) => tpl }
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
