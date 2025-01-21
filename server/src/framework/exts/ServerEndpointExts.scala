package framework.exts

import cats.Applicative
import cats.effect.Temporal
import cats.effect.kernel.Resource.ExitCase
import framework.config.SSEKeepAliveConfig
import framework.tapir.capabilities.ServerSentEvents
import scribe.mdc.MDC
import sttp.capabilities.fs2.Fs2Streams
import sttp.model.sse.ServerSentEvent
import sttp.tapir.{*, given}
import sttp.tapir.server.*
import sttp.tapir.server.http4s.*

import scala.annotation.targetName
import scala.concurrent.duration.FiniteDuration

trait OnSSEStreamFinalzer[F[_]] {
  def applicative: Applicative[F]

  /** The stream has succesfully finished. */
  def onSucceeded(endpoint: Endpoint[?, ?, ?, ?, ?]): F[Unit]

  /** The stream has been cancelled. */
  def onCancelled(endpoint: Endpoint[?, ?, ?, ?, ?]): F[Unit]

  /** The stream has failed. */
  def onError(endpoint: Endpoint[?, ?, ?, ?, ?], error: Throwable): F[Unit]
}
object OnSSEStreamFinalzer {
  given defaultForIO(using
    pkg: sourcecode.Pkg,
    fileName: sourcecode.FileName,
    name: sourcecode.Name,
    line: sourcecode.Line,
    mdc: MDC,
  ): OnSSEStreamFinalzer[IO] = new {
    override def applicative: Applicative[IO] = summon
    override def onSucceeded(endpoint: Endpoint[?, ?, ?, ?, ?]): IO[Unit] = IO.unit
    override def onCancelled(endpoint: Endpoint[?, ?, ?, ?, ?]): IO[Unit] = IO.unit
    override def onError(endpoint: Endpoint[?, ?, ?, ?, ?], error: Throwable): IO[Unit] =
      log.error(s"Error in SSE stream for endpoint ${endpoint.showDetail}", error)
  }
}

// TODO: make this actually fail to compile if the endpoint doesn't have ServerSentEvents capability
// https://softwaremill.community/t/introducing-serversentevents-capability-failing-to-achieve-type-safety/460
extension [SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R <: ServerSentEvents](
  e: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]
) {

  /** Turns the endpoint into a simple server-sent events endpoint.
    *
    * @param keepAlive
    *   a ``
    */
  def toSSE[F[_]: Temporal](keepAlive: Option[SSEKeepAliveConfig])(using
    codec: TapirCodec[String, OUTPUT, ?]
  )(using OnSSEStreamFinalzer[F]): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], Fs2Streams[F]] = {
    e.toSSE(keepAlive) { output =>
      val encoded = codec.encode(output)
      ServerSentEvent(data = Some(encoded))
    }
  }

  /** Turns the endpoint into a simple server-sent events endpoint that serves JSON. */
  def toSSEJson[F[_]: Temporal](keepAlive: Option[SSEKeepAliveConfig])(using
    encoder: CirceEncoder[OUTPUT]
  )(using OnSSEStreamFinalzer[F]): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], Fs2Streams[F]] = {
    e.toSSE(keepAlive) { output =>
      val encoded = encoder(output).noSpaces
      ServerSentEvent(data = Some(encoded))
    }
  }

  /** Turns the endpoint into a simple server-sent events endpoint. */
  def toSSE[F[_]: Temporal](keepAlive: Option[SSEKeepAliveConfig])(
    mapper: OUTPUT => ServerSentEvent
  )(using OnSSEStreamFinalzer[F]): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], Fs2Streams[F]] = {
    e.toSSEStream { stream =>
      val events = stream.map(mapper)
      val keepAliveEvents = keepAlive.fold2(Stream.empty, _.asStream[F])
      events.mergeHaltL(keepAliveEvents)
    }
  }

  /** Turns the endpoint into a simple server-sent events endpoint. */
  def toSSEStream[F[_]](
    pipe: fs2.Pipe[F, OUTPUT, ServerSentEvent]
  )(using
    onStreamError: OnSSEStreamFinalzer[F]
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
        .mapOut[fs2.Stream[F, OUTPUT]](_ => throw new NotImplementedError("this should never be invoked"))(
          pipe(_)
            // It seems that no logging is done if the stream fails, so we need to do it ourselves
            .onFinalizeCase {
              case ExitCase.Succeeded    => onStreamError.onSucceeded(e)
              case ExitCase.Canceled     => onStreamError.onCancelled(e)
              case ExitCase.Errored(err) => onStreamError.onError(e, err)
            }(using onStreamError.applicative)
        )
    endpoint
  }
}
