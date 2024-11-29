package framework.exts

import sttp.tapir.*
import sttp.tapir.server.*
import sttp.tapir.server.http4s.*
import sttp.model.sse.ServerSentEvent
import sttp.capabilities.fs2.Fs2Streams
import scala.annotation.targetName
import sttp.tapir.given

extension [SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R](
  e: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]
) {

  /** Turns the endpoint into a simple server-sent events endpoint. */
  def toSSE[F[_]](using
    codec: TapirCodec[String, OUTPUT, TapirCodecFormat.TextPlain]
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], R & Fs2Streams[F]] = {
    e.toSSE { output =>
      val encoded = codec.encode(output)
      ServerSentEvent(data = Some(encoded))
    }
  }

  /** Turns the endpoint into a simple server-sent events endpoint. */
  def toSSE[F[_]](
    mapper: OUTPUT => ServerSentEvent
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], R & Fs2Streams[F]] = {
    e.toSSEStream(_.map(mapper))
  }

  /** Turns the endpoint into a simple server-sent events endpoint. */
  def toSSEStream[F[_]](
    pipe: fs2.Pipe[F, OUTPUT, ServerSentEvent]
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], R & Fs2Streams[F]] = {
    val sseBody = serverSentEventsBody[F]

    // val e1 = sttp.tapir.endpoint.get.out(sseBody)
    // e1.serverSecurityLogicSuccess[Unit, IO](???).serverLogicSuccess()

    val sseEndpoint: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, ServerSentEvent], R & Fs2Streams[F]] =
      e.withOutputPublic(sseBody.toEndpointIO)
    val endpoint =
      sseEndpoint
        .mapOut[fs2.Stream[F, OUTPUT]](_ => throw new NotImplementedError("this should never be invoked"))(pipe)
    endpoint
  }
}
