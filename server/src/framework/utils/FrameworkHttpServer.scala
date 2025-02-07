package framework.utils

import cats.data.Kleisli
import cats.syntax.all.*
import framework.config.HttpConfig
import framework.data.FrameworkDateTime
import framework.http.middleware.*
import framework.prelude.*
import fs2.io.net.tls.TLSContext
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.otel4s.middleware.metrics.OtelMetrics
import org.http4s.otel4s.middleware.redact.{PathRedactor, QueryRedactor}
import org.http4s.otel4s.middleware.trace.server.{PathAndQueryRedactor, RouteClassifier, ServerMiddlewareBuilder}
import org.http4s.server.middleware.Metrics
import org.http4s.server.{ContextMiddleware, HttpMiddleware, Router, Server}
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider
import sttp.tapir.server.http4s.Http4sServerInterpreter

import scala.util.chaining.*

object FrameworkHttpServer {
  object Routes {

    /** All routes defined in this object. */
    def all(using StreamRegistry[IO]): HttpRoutes[IO] =
      health <+> streamStats

    /** Returns a route that returns a 200 OK response on `/api/health`. */
    def health: HttpRoutes[IO] =
      HttpRoutes.of[IO] { case GET -> Root / "api" / "health" => Ok("OK") }

    /** Returns a route that returns response on `/api/stats/streams`. */
    def streamStats(using streamRegistry: StreamRegistry[IO]): HttpRoutes[IO] =
      HttpRoutes.of[IO] { case GET -> Root / "api" / "stats" / "streams" =>
        streamRegistry.get.flatMap(map => Ok(Map("streams" -> map).asJson))
      }
  }

  /** Returns a context middleware that extracts the request. */
  def reqContextMiddleware[Context](f: Request[IO] => IO[Context]): ContextMiddleware[IO, Context] =
    ContextMiddleware(Kleisli { (req: Request[IO]) =>
      OptionT.liftF(f(req))
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

  /** Creates a HTTP server with CORS, logging, metrics and tracing.
    *
    * @param extraRoutes
    *   additional routes to attach, like those in [[Routes]]. These routes won't have client tracing applied. Use
    *   [[HttpRoutes.empty]] to not add any extra routes.
    */
  def serverResource[Context](
    cfg: HttpConfig,
    contextMiddleware: ContextMiddleware[IO, Context],
    createRoutes: GetCurrentRequest[IO] ?=> Http4sServerInterpreter[IO] => ContextRoutes[Context, IO],
    otelMiddleware: ServerMiddlewareBuilder[IO],
    extraRoutes: HttpRoutes[IO],
    clientSpanName: Request[IO] => String = defaultSpanNameForClient,
  )(using TracerProvider[IO], MeterProvider[IO]): Resource[IO, Server] = {
    val serverInterpreter = Http4sServerInterpreter[IO]()
    val loggerMiddleware = cfg.logging.logger()

    def routesPipeline(
      metricsMiddleware: HttpMiddleware[IO],
      currentReq: GetOrStoreCurrentRequest[IO],
      withClientTracing: Boolean,
    )(
      routes: HttpRoutes[IO]
    ): IO[HttpRoutes[IO]] = {
      val service = routes
        .pipe(currentReq.middleware)
        .pipe(cfg.corsPolicy.apply)
        .pipe(loggerMiddleware.apply)
        .pipe(metricsMiddleware.apply)

      for {
        service <- otelMiddleware.buildHttpRoutes(service)
        // Order matters here, the client tracing middleware needs to be applied first.
        service <-
          if (withClientTracing)
            ClientRequestTracingMiddleware[IO](
              maxDriftFromCurrentTime = cfg.clientRequestTracing.maxDriftFromCurrentTime,
              spanName = clientSpanName,
            )(service)
          else IO.pure(service)
      } yield service
    }

    for {
      metricsOps <- (
        OtelMetrics.serverMetricsOps[IO]() <* log.info("Created HTTP server metrics under `http.` namespace.")
      ).toResource
      currentReq <- GetOrStoreCurrentRequest.create.toResource
      metricsMiddleware = Metrics(metricsOps)
      ctxRoutes = createRoutes(using currentReq)(serverInterpreter).pipe(contextMiddleware)
      ctxRoutes <- routesPipeline(metricsMiddleware, currentReq, withClientTracing = true)(ctxRoutes).toResource
      extraRoutes <-
        routesPipeline(metricsMiddleware, currentReq, withClientTracing = false)(extraRoutes).toResource
      // Note the order matters here, as `extraRoutes` are more lax and `ctxRoutes` needs certain headers to be set and
      // fails requests if they are missing.
      httpApp = (extraRoutes <+> ctxRoutes).orNotFound
      maybeTls <- cfg.server.tls
        .map(tlsConfig =>
          TLSContext.Builder
            .forAsync[IO]
            .fromKeyStoreFile(
              tlsConfig.keyStoreFile.toAbsolutePath(),
              storePassword = tlsConfig.keyStorePassword.toCharArray(),
              keyPassword = tlsConfig.keyPassword.toCharArray(),
            )
        )
        .sequence
        .toResource
      server <-
        EmberServerBuilder
          .default[IO]
          .withHost(cfg.server.host)
          .withPort(cfg.server.port)
          .withHttpApp(httpApp)
          .pipe(b => if (cfg.server.useHttp2) b.withHttp2 else b.withoutHttp2)
          .pipe(b => maybeTls.fold(b)(b.withTLS(_)))
          .build
    } yield server
  }
}
