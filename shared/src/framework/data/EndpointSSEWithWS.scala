package framework.data

import framework.prelude.*
import cats.effect.IO
import framework.tapir.capabilities.ServerSentEvents
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.tapir.Endpoint
import cats.effect.Concurrent
import cats.Show
import cats.data.NonEmptyVector
import scala.concurrent.duration.*

/** A Tapir SSE endpoint that also has a websocket alternative. */
case class EndpointSSEWithWS[F[_]: Concurrent, SecurityInput, Input, Output, AuthError, -Requirements](
  sse: Endpoint[SecurityInput, Input, AuthError, Output, Requirements & ServerSentEvents],
  webSocket: Endpoint[
    SecurityInput,
    Input,
    AuthError,
    fs2.Pipe[F, Nothing, Output],
    Requirements & Fs2Streams[F] & WebSockets,
  ],
) {

  /** [[webSocket]] adapted to have the same output as [[sse]]. */
  val webSocketAsSSE: Endpoint[
    SecurityInput,
    Input,
    AuthError,
    fs2.Stream[F, Output],
    Requirements & Fs2Streams[F] & WebSockets,
  ] = webSocket.mapOut(pipe => pipe(fs2.Stream.empty))(outputStream =>
    inputStream => inputStream.mergeHaltBoth(outputStream)
  )
}
object EndpointSSEWithWS {
  enum ClientConnectionMode derives CanEqual, CirceCodec {

    /** Connect via SSE. */
    case ServerSentEvents

    /** Connect via WebSocket. */
    case WebSocket

    /** Connect via SSE as a primary way, falling back to WebSocket if the connection failed.
      *
      * @param trySSEAgainAt
      *   If set, forces WebSocket until this time, then tries SSE again. [[None]] means normal behavior (try SSE
      *   first).
      */
    case SSEWithWebSocketFallback(trySSEAgainAt: Option[FrameworkDateTime] = None)
  }
  object ClientConnectionMode {
    given Show[ClientConnectionMode] = Show.fromToString

    val defaultRecoveryTimeout: FiniteDuration = 24.hours
  }

  /** Simplified enum for UI selection, without the timestamp parameter. */
  enum ClientConnectionModeKind derives CanEqual {
    case ServerSentEvents
    case WebSocket
    case SSEWithWebSocketFallback

    def toMode: ClientConnectionMode = this match {
      case ServerSentEvents         => ClientConnectionMode.ServerSentEvents
      case WebSocket                => ClientConnectionMode.WebSocket
      case SSEWithWebSocketFallback => ClientConnectionMode.SSEWithWebSocketFallback()
    }
  }
  object ClientConnectionModeKind {
    given Show[ClientConnectionModeKind] = Show.fromToString

    val neValues: NonEmptyVector[ClientConnectionModeKind] = NonEmptyVector.fromVectorUnsafe(Vector(values*))

    def fromMode(mode: ClientConnectionMode): ClientConnectionModeKind = mode match {
      case ClientConnectionMode.ServerSentEvents            => ServerSentEvents
      case ClientConnectionMode.WebSocket                   => WebSocket
      case ClientConnectionMode.SSEWithWebSocketFallback(_) => SSEWithWebSocketFallback
    }
  }
}
