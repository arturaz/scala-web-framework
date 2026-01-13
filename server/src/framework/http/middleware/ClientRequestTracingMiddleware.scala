package framework.http.middleware

import cats.Monad
import cats.data.Kleisli
import cats.effect.kernel.Clock
import framework.api.FrameworkHeaders
import framework.data.FrameworkDateTime
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Accept
import org.http4s.otel4s.middleware.instances.all.*
import org.http4s.server.HttpMiddleware
import org.typelevel.ci.CIString
import org.typelevel.otel4s.trace.{SpanKind, TracerProvider}

import scala.concurrent.duration.FiniteDuration

/** Starts tracing spans based on client request time. */
object ClientRequestTracingMiddleware { self =>

  /** How to handle server-sent events and websocket requests. */
  enum StreamingHandlingMode derives CanEqual {

    /** Streaming requests are not traced. */
    case Ignore

    /** Get the header value from the query parameters. */
    case GetHeaderFromQueryParameters
  }

  /** @param maxDriftFromCurrentTime
    *   the maximum amount of time that we allow the client request time to drift from the current time. This is needed
    *   because we can't trust the client to always provide the current time, either due to malice or due to bad clock
    *   settings on the client.
    * @param spanName
    *   a function that returns a span name based on the request.
    * @param headerName
    *   the name of the header that contains the client request time as a unix timestamp.
    * @param failOnInvalidHeader
    *   whether to fail the request if the header is not present.
    * @param service
    *   the service to wrap.
    * @param tracer
    *   the tracer to use.
    * @param clock
    *   the clock to use.
    * @return
    *   the wrapped service.
    */
  def apply[F[_]: Monad](
    maxDriftFromCurrentTime: FiniteDuration,
    spanName: Request[F] => String,
    headerName: CIString = CIString(FrameworkHeaders.`X-Request-Started-At`.Name),
    failOnInvalidHeader: Boolean = true,
    streamingHandlingMode: StreamingHandlingMode = StreamingHandlingMode.GetHeaderFromQueryParameters,
  )(using tracerProvider: TracerProvider[F], clock: Clock[F]): F[HttpMiddleware[F]] = {
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    val now = clock.realTimeInstant.map(FrameworkDateTime.fromInstant)

    tracerProvider.tracer(sourcecode.FullName.here).get.map { tracer => service =>
      Kleisli { (req: Request[F]) =>
        val isStreamingRequest = isSSERequest(req) || isWebSocketRequest(req)

        def getHeaderFromQueryParameters = (
          req.uri.query.params.get(headerName.toString).map(value => Header.Raw(headerName, value)),
          // Remove the "header" from query parameters to not pollute our spans.
          req.withUri(req.uri.removeQueryParam(headerName.toString)),
        )

        def getHeaderFromRequest = (req.headers.get(`headerName`).map(_.head), req)

        def getHeader = if (isStreamingRequest) getHeaderFromQueryParameters else getHeaderFromRequest

        // `OPTIONS` requests usually do not include custom headers from the browser, so just let them through.
        if (req.method == Method.OPTIONS) service(req)
        else if (isStreamingRequest && streamingHandlingMode == StreamingHandlingMode.Ignore) service(req)
        else {
          val (maybeRawHeader, req) = getHeader
          val maybeHeader = maybeRawHeader.map(parseHeader)

          maybeHeader match {
            case None =>
              if (failOnInvalidHeader) OptionT.liftF(BadRequest(s"Missing header: ${`headerName`}"))
              else service(req)

            case Some(Left(error)) =>
              if (failOnInvalidHeader) OptionT.liftF(BadRequest(error))
              else service(req)

            case Some(Right(at)) =>
              val io = for {
                now <- now
                adjusted = adjustForDrift(now, at, maxDriftFromCurrentTime)
                spanOps = tracer
                  .spanBuilder(show"client: ${spanName(req)}")
                  .withSpanKind(SpanKind.Client)
                  .withStartTimestamp(adjusted.toFiniteDuration)
                  .build
                maybeResponse <- spanOps.surround(
                  // Propagate the headers to the service so that Otel4s-Http4s integration can use it.
                  tracer.propagate(req.headers).map(req.withHeaders).flatMap(service(_).value)
                )
              } yield maybeResponse

              OptionT(io)
          }
        }
      }
    }
  }

  def isSSERequest[F[_]](req: Request[F]): Boolean =
    req.method == Method.GET && (req.headers.get[headers.Accept] match {
      case None         => false
      case Some(accept) => accept.values.exists(_.mediaRange == MediaType.`text/event-stream`)
    })

  def isWebSocketRequest[F[_]](req: Request[F]): Boolean =
    req.headers
      .get(headers.Upgrade.headerInstance.name)
      .exists(_.head.value.equalsIgnoreCase("websocket"))

  def parseHeader(header: Header.Raw): Either[String, FrameworkDateTime] = {
    header.value.toLongOption
      .toRight(show"Invalid header value for ${header.name}: ${header.value}")
      .map(FrameworkDateTime.fromUnixMillis)
  }

  /** Makes sure that the client request time is within `maxDriftFromCurrentTime` from `now`. */
  def adjustForDrift(now: FrameworkDateTime, at: FrameworkDateTime, maxDrift: FiniteDuration): FrameworkDateTime = {
    val diff = now - at

    if (diff > maxDrift) now - maxDrift
    else if (diff < -maxDrift) now + maxDrift
    else at
  }
}
