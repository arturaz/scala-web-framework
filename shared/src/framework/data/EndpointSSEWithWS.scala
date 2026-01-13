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

    /** Connect via SSE as a primary way, falling back to WebSocket if the connection failed. */
    case SSEWithWebSocketFallback
  }
  object ClientConnectionMode {
    given Show[ClientConnectionMode] = Show.fromToString

    val neValues: NonEmptyVector[ClientConnectionMode] = NonEmptyVector.fromVectorUnsafe(Vector(values*))
  }
}
