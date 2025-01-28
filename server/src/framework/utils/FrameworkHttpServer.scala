package framework.utils

import cats.data.Kleisli
import cats.syntax.all.*
import framework.config.HttpConfig
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.otel4s.middleware.metrics.OtelMetrics
import org.http4s.otel4s.middleware.redact.{PathRedactor, QueryRedactor}
import org.http4s.otel4s.middleware.trace.server.{PathAndQueryRedactor, ServerMiddlewareBuilder}
import org.http4s.server.middleware.Metrics
import org.http4s.server.{ContextMiddleware, Router, Server}
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider
import sttp.tapir.server.http4s.Http4sServerInterpreter

import scala.util.chaining.*
import org.http4s.otel4s.middleware.trace.server.RouteClassifier
import framework.http.middleware.ClientRequestTracingMiddleware
import framework.data.FrameworkDateTime

object FrameworkHttpServer {

  /** Returns a route that returns a 200 OK response on `/api/health`. */
  def healthRoute: HttpRoutes[IO] =
    HttpRoutes.of[IO] { case GET -> Root / "api" / "health" => Ok("OK") }

  /** Returns a context middleware that extracts the host from the request and passes it to the given function. */
  def contextMiddleware[Context](f: headers.Host => IO[Context]): ContextMiddleware[IO, Context] =
    ContextMiddleware(Kleisli { (req: Request[IO]) =>
      val io = req.headers.get[headers.Host] match {
        case None       => IO.raiseError(new Exception("Host header is required."))
        case Some(host) => f(host)
      }
      OptionT.liftF(io)
    })

  /** Does not redact anything. */
  def neverRedactor: PathAndQueryRedactor = new PathRedactor.NeverRedact with QueryRedactor.NeverRedact {}

  def defaultSpanNameForClient(req: Request[?]): String =
    show"${req.method} ${req.httpVersion} ${req.uri}"

  def defaultSpanNameForServer(req: Request[?]): String =
    // Middleware adds the request method automatically.
    show"${req.httpVersion} ${req.uri}"

  /** The default OpenTelemetry middleware builder that does not redact anything. */
  def otelMiddleware(using TracerProvider[IO]): ServerMiddlewareBuilder[IO] =
    ServerMiddlewareBuilder
      .default[IO](neverRedactor)
      .withRouteClassifier(RouteClassifier.of { case req => defaultSpanNameForServer(req) })

  /** Creates a HTTP server with CORS, logging, metrics and tracing. */
  def serverResource[Context](
    cfg: HttpConfig,
    contextMiddleware: ContextMiddleware[IO, Context],
    createRoutes: Http4sServerInterpreter[IO] => ContextRoutes[Context, IO],
    otelMiddleware: ServerMiddlewareBuilder[IO],
    extraRoutes: HttpRoutes[IO] = healthRoute,
    clientSpanName: Request[IO] => String = defaultSpanNameForClient,
  )(using TracerProvider[IO], MeterProvider[IO]): Resource[IO, Server] = {
    val serverInterpreter = Http4sServerInterpreter[IO]()
    val ctxRoutes = createRoutes(serverInterpreter)
    val appliedCtxRoutes = contextMiddleware(ctxRoutes)
    val router = Router("/" -> (appliedCtxRoutes <+> extraRoutes))

    for {
      metricsOps <- OtelMetrics.serverMetricsOps[IO]().toResource
      service = router
        .pipe(cfg.corsPolicy.apply)
        .pipe(cfg.logging.logger().apply)
        .pipe(Metrics(metricsOps).apply)
      service <- otelMiddleware.buildHttpRoutes(service).toResource
      // Order matters here, the client tracing middleware needs to be applied first.
      service <- ClientRequestTracingMiddleware[IO](
        maxDriftFromCurrentTime = cfg.clientRequestTracing.maxDriftFromCurrentTime,
        spanName = clientSpanName,
      )(service).toResource
      httpApp = service.orNotFound
      server <-
        EmberServerBuilder
          .default[IO]
          .withHost(cfg.server.host)
          .withPort(cfg.server.port)
          .withHttpApp(httpApp)
          .build
    } yield server
  }
}
