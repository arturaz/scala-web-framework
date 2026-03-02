package framework.api

import framework.data.{EndpointSSEWithWS, FrameworkDateTime}
import framework.exts.*
import framework.prelude.{*, given}
import framework.tapir.FrameworkTapir.circeCodec
import sttp.tapir.{endpoint, stringToPath, EndpointInput}

/** Framework-level endpoints that are not domain-specific to the app. */
class FrameworkEndpoints(
  val RootPath: EndpointInput.FixedPath[Unit]
) extends FrameworkEndpoints.SseConnectionTryout {

  /** Unauthenticated connectivity tryout endpoint for selecting SSE vs WebSocket transport on the client. */
  val sseConnectionTryout: EndpointSSEWithWS[IO, Unit, Unit, FrameworkDateTime, Unit, Any] =
    endpoint.get
      .in(RootPath / "framework" / "sse-connection-tryout")
      .serverSentEventsAndWebSocket[FrameworkDateTime]()
}
object FrameworkEndpoints {
  trait SseConnectionTryout {
    def sseConnectionTryout: EndpointSSEWithWS[IO, Unit, Unit, FrameworkDateTime, Unit, Any]
  }
}
