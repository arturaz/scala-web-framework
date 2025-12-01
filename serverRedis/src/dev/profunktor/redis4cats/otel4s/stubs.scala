package dev.profunktor.redis4cats.otel4s

import dev.profunktor.redis4cats.pubsub.SubscribeCommands
import dev.profunktor.redis4cats.data.RedisChannel
import org.typelevel.otel4s.trace.SpanOps
import dev.profunktor.redis4cats.data.RedisPattern
import dev.profunktor.redis4cats.data.RedisPatternEvent
import cats.Functor
import cats.syntax.all.*
import framework.prelude.*
import org.typelevel.otel4s.trace.StubSpanOps
import dev.profunktor.redis4cats.pubsub.PubSubCommands
import dev.profunktor.redis4cats.pubsub.PublishCommands
import org.typelevel.otel4s.trace.SpanBuilder
import dev.profunktor.redis4cats.streams.Streaming
import dev.profunktor.redis4cats.effects.XReadOffsets
import scala.concurrent.duration.Duration
import dev.profunktor.redis4cats.RestartOnTimeout
import dev.profunktor.redis4cats.effects.StreamMessage
import dev.profunktor.redis4cats.RedisCommands
import org.typelevel.otel4s.trace.TracerProvider
import cats.Applicative

object TracedRedisCommands {

  /** Constructor for [[TracerProvider]]. */
  def apply[F[_]: Applicative: Functor, K, V](
    cmd: RedisCommands[F, K, V],
    config: TracedRedisConfig[F, K, V],
  )(implicit tracerProvider: TracerProvider[F]): F[TracedRedisCommands[F, K, V]] = {
    cmd.pure
  }
}
type TracedRedisCommands[F[_], K, V] = RedisCommands[F, K, V]

extension [A](value: A) {

  def withWrapper[F[_]](f: CommandWrapper[F] => CommandWrapper[F]): A =
    value

}

/** Stub implementation to prevent changing the code until we get otel4s-redis4cats back on track. */
type TracedSubscribeCommands[F[_], S[_], K, V] = SubscribeCommands[F, S, K, V]

extension [F[_], S[_], K, V](cmd: TracedSubscribeCommands[F, S, K, V]) {

  /** As [[subscribe]] but returns `SpanOps` for each event that introduces a span when it is used.
    *
    * You must use the `SpanOps` yourself to record the span.
    */
  def subscribeWithTracedEvents(
    channel: RedisChannel[K],
    eventName: V => String = (_: V) => "event",
  )(using Applicative[F], Functor[S]): S[(SpanOps[F], V)] = cmd.subscribe(channel).map { v => (new StubSpanOps[F], v) }

  /** As [[psubscribe]] but returns `SpanOps` for each event that introduces a span when it is used.
    *
    * You must use the `SpanOps` yourself to record the span.
    */
  def psubscribeWithTracedEvents(
    channel: RedisPattern[K],
    eventName: RedisPatternEvent[K, V] => String = (_: RedisPatternEvent[K, V]) => "event",
  )(using Applicative[F], Functor[S]): S[(SpanOps[F], RedisPatternEvent[K, V])] = cmd.psubscribe(channel).map { v =>
    (new StubSpanOps[F], v)
  }
}

/** Stub implementation to prevent changing the code until we get otel4s-redis4cats back on track. */
type TracedPublishCommands[F[_], S[_], K, V] = PublishCommands[F, S, K, V]

/** Stub implementation to prevent changing the code until we get otel4s-redis4cats back on track. */
type TracedPubSubCommands[F[_], S[_], K, V] = PubSubCommands[F, S, K, V]
object TracedPubSubCommands {

  /** Constructor for [[TracerProvider]]. */
  def apply[F[_]: Applicative: Functor, S[_], K, V](
    cmd: PubSubCommands[F, S, K, V],
    config: TracedRedisConfig[F, K, V],
  )(implicit tracerProvider: TracerProvider[F], SFunctor: Functor[S]): F[TracedPubSubCommands[F, S, K, V]] = {
    cmd.pure
  }
}

case class TracedRedisConfig[F[_], K, V](
  spanName: String => String,
  configureSpanBuilder: (SpanBuilder[F], String) => SpanBuilder[F],
  recordKey: Option[K => String],
  recordValue: Option[V => String],
)

type TracedStreaming[F[_], S[_], K, V] = Streaming[F, S, K, V]
object TracedStreaming {

  /** Constructor for [[TracerProvider]]. */
  def apply[F[_]: Applicative: Functor, S[_], K, V](
    cmd: Streaming[F, S, K, V],
    config: TracedRedisConfig[F, K, V],
  )(implicit tracerProvider: TracerProvider[F], SFunctor: Functor[S]): F[TracedStreaming[F, S, K, V]] = {
    cmd.pure
  }
}

extension [F[_], S[_], K, V](cmd: TracedStreaming[F, S, K, V]) {

  /** As [[read]] but returns `SpanOps` for each message that introduces a span when it is used.
    *
    * You must use the `SpanOps` yourself to record the span.
    */
  def readWithTracedMessages(
    streams: Set[XReadOffsets[K]],
    block: Option[Duration] = Some(Duration.Zero),
    count: Option[Long] = None,
    restartOnTimeout: RestartOnTimeout = RestartOnTimeout.always,
    spanName: StreamMessage[K, V] => String = (_: StreamMessage[K, V]) => "message",
  )(using Applicative[F], Functor[S]): S[(SpanOps[F], StreamMessage[K, V])] =
    cmd.read(streams, block, count, restartOnTimeout).map { v => (new StubSpanOps[F], v) }

}
