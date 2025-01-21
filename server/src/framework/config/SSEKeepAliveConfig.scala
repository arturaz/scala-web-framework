package framework.config

import cats.effect.Temporal
import sttp.model.sse.ServerSentEvent

import scala.concurrent.duration.FiniteDuration

/** Configuration for server-sent events keep-alive.
  *
  * @param interval
  *   the interval between keep-alive messages
  * @param eventType
  *   the event type to send for the keep-alive messages
  * @param data
  *   the data to send for the keep-alive messages
  */
final case class SSEKeepAliveConfig(
  interval: FiniteDuration,
  eventType: String = "keep-alive",
  data: Option[String] = None,
) {

  /** Returns a single keep-alive message. */
  def asEvent: ServerSentEvent = ServerSentEvent(data = data, eventType = Some(eventType))

  /** Returns a stream of keep-alive messages. */
  def asStream[F[_]: Temporal]: fs2.Stream[F, ServerSentEvent] = {
    val evt = asEvent
    fs2.Stream.fixedDelay(interval).map(_ => evt)
  }
}
