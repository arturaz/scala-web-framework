package framework.data

import cats.effect.IO
import framework.tapir.capabilities.ServerSentEvents
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.tapir.Endpoint

/** A Tapir SSE endpoint that also has a websocket alternative. */
case class EndpointSSEWithWS[SecurityInput, Input, Output2, AuthError, Requirements](
  sse: Endpoint[SecurityInput, Input, AuthError, Output2, Requirements & ServerSentEvents],
  webSocket: Endpoint[
    SecurityInput,
    Input,
    AuthError,
    fs2.Pipe[IO, Nothing, Output2],
    Requirements & Fs2Streams[IO] & WebSockets,
  ],
) {

  /** [[webSocket]] adapted to have the same output as [[sse]]. */
  def webSocketAsSSE: Endpoint[
    SecurityInput,
    Input,
    AuthError,
    fs2.Stream[IO, Output2],
    Requirements & Fs2Streams[IO] & WebSockets,
  ] = webSocket.mapOut(pipe => pipe(fs2.Stream.empty))(outputStream => inputStream => inputStream ++ outputStream)
}
