package framework.exts

import sttp.tapir.*
import sttp.tapir.server.*
import sttp.tapir.server.http4s.*
import sttp.model.sse.ServerSentEvent
import sttp.capabilities.fs2.Fs2Streams
import scala.annotation.targetName
import sttp.tapir.given
import framework.tapir.capabilities.ServerSentEvents

// TODO: make this actually fail to compile if the endpoint doesn't have ServerSentEvents capability
// https://softwaremill.community/t/introducing-serversentevents-capability-failing-to-achieve-type-safety/460
extension [SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R <: ServerSentEvents](
  e: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]
) {

  /** Turns the endpoint into a simple server-sent events endpoint. */
  def toSSE[F[_]](using
    codec: TapirCodec[String, OUTPUT, ?]
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], Fs2Streams[F]] = {
    e.toSSE { output =>
      val encoded = codec.encode(output)
      ServerSentEvent(data = Some(encoded))
    }
  }

  /** Turns the endpoint into a simple server-sent events endpoint that serves JSON. */
  def toSSEJson[F[_]](using
    encoder: CirceEncoder[OUTPUT]
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], Fs2Streams[F]] = {
    e.toSSE { output =>
      val encoded = encoder(output).noSpaces
      ServerSentEvent(data = Some(encoded))
    }
  }

  /** Turns the endpoint into a simple server-sent events endpoint. */
  def toSSE[F[_]](
    mapper: OUTPUT => ServerSentEvent
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], Fs2Streams[F]] = {
    e.toSSEStream(_.map(mapper))
  }

  /** Turns the endpoint into a simple server-sent events endpoint. */
  def toSSEStream[F[_]](
    pipe: fs2.Pipe[F, OUTPUT, ServerSentEvent]
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], Fs2Streams[F]] = {
    val sseBody = serverSentEventsBody[F]

    val sseEndpoint =
      e.withOutputPublic(sseBody.toEndpointIO)
        .asInstanceOf[Endpoint[
          SECURITY_INPUT,
          INPUT,
          ERROR_OUTPUT,
          Stream[F, ServerSentEvent],
          Fs2Streams[F],
        ]]
    val endpoint =
      sseEndpoint
        .mapOut[fs2.Stream[F, OUTPUT]](_ => throw new NotImplementedError("this should never be invoked"))(pipe)
    endpoint
  }
}
