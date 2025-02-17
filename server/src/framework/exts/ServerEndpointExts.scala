package framework.exts

import cats.Applicative
import cats.effect.Temporal
import cats.effect.kernel.Resource.ExitCase
import framework.config.SSEKeepAliveConfig
import framework.tapir.capabilities.ServerSentEvents
import scribe.mdc.MDC
import sttp.capabilities.fs2.Fs2Streams
import sttp.model.sse.ServerSentEvent
import sttp.tapir.{AttributeKey => _, *, given}
import sttp.tapir.server.*
import sttp.tapir.server.http4s.*

import scala.annotation.targetName
import scala.concurrent.duration.FiniteDuration
import framework.utils.StreamRegistry
import framework.utils.StreamRegistry.StreamDetails
import org.typelevel.otel4s.AttributeKey
import framework.http.middleware.GetCurrentRequest
import framework.data.AttributeWithCirceEncoder
import org.http4s.Request
import java.io.IOException

trait SSEStreamFinalizer[F[_]] {
  def applicative: Applicative[F]

  /** The stream has succesfully finished. */
  def onSucceeded(endpoint: Endpoint[?, ?, ?, ?, ?]): F[Unit]

  /** The stream has been cancelled. */
  def onCancelled(endpoint: Endpoint[?, ?, ?, ?, ?]): F[Unit]

  /** The stream has failed due to a broken pipe (the client has closed the connection and we tried to send something).
    */
  def onBrokenPipe(endpoint: Endpoint[?, ?, ?, ?, ?]): F[Unit]

  /** The stream has failed. */
  def onError(endpoint: Endpoint[?, ?, ?, ?, ?], error: Throwable): F[Unit]
}
object SSEStreamFinalizer {
  given defaultForIO(using
    pkg: sourcecode.Pkg,
    fileName: sourcecode.FileName,
    name: sourcecode.Name,
    line: sourcecode.Line,
    mdc: MDC,
  ): SSEStreamFinalizer[IO] = new {
    override def applicative: Applicative[IO] = summon
    override def onSucceeded(endpoint: Endpoint[?, ?, ?, ?, ?]): IO[Unit] = IO.unit
    override def onCancelled(endpoint: Endpoint[?, ?, ?, ?, ?]): IO[Unit] =
      log.debug(s"Cancelled SSE stream for endpoint ${endpoint.showDetail}")

    override def onBrokenPipe(endpoint: Endpoint[?, ?, ?, ?, ?]): IO[Unit] =
      log.debug(s"Broken pipe (client disconnected) in SSE stream for endpoint ${endpoint.showDetail}")

    override def onError(endpoint: Endpoint[?, ?, ?, ?, ?], error: Throwable): IO[Unit] =
      log.error(s"Error in SSE stream for endpoint ${endpoint.showDetail}", error)
  }
}

// TODO: make this actually fail to compile if the endpoint doesn't have ServerSentEvents capability
// https://softwaremill.community/t/introducing-serversentevents-capability-failing-to-achieve-type-safety/460
extension [SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R <: ServerSentEvents](
  e: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]
) {

  /** Turns the endpoint into a simple server-sent events endpoint. */
  def toSSE[F[_]: Temporal](keepAlive: Option[SSEKeepAliveConfig])(using
    codec: TapirCodec[String, OUTPUT, ?]
  )(using
    SSEStreamFinalizer[F],
    StreamRegistry[F],
    GetCurrentRequest[F],
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], Fs2Streams[F]] = {
    e.toSSE(keepAlive) { output =>
      val encoded = codec.encode(output)
      ServerSentEvent(data = Some(encoded))
    }
  }

  /** Turns the endpoint into a simple server-sent events endpoint that serves JSON. */
  def toSSEJson[F[_]: Temporal](keepAlive: Option[SSEKeepAliveConfig])(using
    encoder: CirceEncoder[OUTPUT]
  )(using
    SSEStreamFinalizer[F],
    StreamRegistry[F],
    GetCurrentRequest[F],
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], Fs2Streams[F]] = {
    e.toSSE(keepAlive) { output =>
      val encoded = encoder(output).noSpaces
      ServerSentEvent(data = Some(encoded))
    }
  }

  /** Turns the endpoint into a simple server-sent events endpoint. */
  def toSSE[F[_]: Temporal](keepAlive: Option[SSEKeepAliveConfig])(
    mapper: OUTPUT => ServerSentEvent
  )(using
    SSEStreamFinalizer[F],
    StreamRegistry[F],
    GetCurrentRequest[F],
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], Fs2Streams[F]] = {
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
    streamFinalizer: SSEStreamFinalizer[F],
    registry: StreamRegistry[F],
    getRequest: GetCurrentRequest[F],
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, Stream[F, OUTPUT], Fs2Streams[F]] = {
    val sseBody = serverSentEventsBody[F]
    val streamName = StreamRegistry.StreamName.fromEndpoint("SSE: ", e)

    def attributesForReq(req: Request[F]) = {
      AttributeWithCirceEncoder.fromAttribute(AttributeKey.string("method")(req.method.show)) ::
        AttributeWithCirceEncoder.fromAttribute(AttributeKey.string("uri")(req.uri.show)) ::
        AttributeWithCirceEncoder.fromAttribute(
          AttributeKey.string("httpVersion")(req.httpVersion.show)
        ) ::
        req.isSecure.map { isSecure =>
          AttributeWithCirceEncoder.fromAttribute(AttributeKey.boolean("isSecure")(isSecure))
        }.toList :::
        req.server.map { server =>
          AttributeWithCirceEncoder.fromAttribute(AttributeKey.string("server")(server.show))
        }.toList :::
        req.remote.map { remote =>
          AttributeWithCirceEncoder.fromAttribute(AttributeKey.string("remote")(remote.show))
        }.toList :::
        req.headers.redactSensitive().headers.map { header =>
          AttributeWithCirceEncoder
            .fromAttribute(AttributeKey.string(show"header.${header.name}")(header.value))
        }
    }

    /** It seems that no logging is done if the stream fails, so we need to do it ourselves. */
    def logErrors[A]: fs2.Pipe[F, A, A] =
      _.onFinalizeCase {
        case ExitCase.Succeeded                                                      => streamFinalizer.onSucceeded(e)
        case ExitCase.Canceled                                                       => streamFinalizer.onCancelled(e)
        case ExitCase.Errored(err: IOException) if err.getMessage() == "Broken pipe" => streamFinalizer.onBrokenPipe(e)
        case ExitCase.Errored(err)                                                   => streamFinalizer.onError(e, err)
      }(using streamFinalizer.applicative)

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
        .mapOut[fs2.Stream[F, OUTPUT]](_ => throw new NotImplementedError("this should never be invoked")) { stream =>
          for {
            maybeReq <- Stream.eval(getRequest.tryGet)
            maybeStreamDetails = maybeReq.map { req =>
              val attributes = attributesForReq(req)
              StreamDetails(attributes*)
            }
            response <- registry.register(streamName, maybeStreamDetails, stream.through(pipe)).through(logErrors)
          } yield response
        }

    endpoint
  }
}
