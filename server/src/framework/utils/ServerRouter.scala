package framework.utils

import framework.http.middleware.GetCurrentRequest
import org.http4s.ContextRoutes
import org.http4s.server.websocket.WebSocketBuilder2
import cats.Monad
import cats.syntax.all.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import scala.reflect.ClassTag
import sttp.tapir.server.ServerEndpoint
import sttp.capabilities.fs2.Fs2Streams
import sttp.tapir.server.http4s.Context as TapirHttp4sContext
import sttp.capabilities.WebSockets
import scala.annotation.targetName
import sttp.tapir.Endpoint
import sttp.tapir.server.http4s.RichHttp4sEndpoint
import sttp.tapir.typelevel.ParamConcat
import framework.data.EndpointSSEWithWS
import sttp.tapir.EndpointServerLogicOps
import framework.tapir.capabilities.ServerSentEvents

/** Helper trait for creating routes from endpoints.
  *
  * Example:
  * {{{
  * import framework.utils.{ServerRouter as FrameworkServerRouter}
  *
  * class ServerRouter(using Transactor[IO]) extends FrameworkServerRouter[ServerContext] {
  *    override def routes(
  *      builder: FrameworkServerRouter.Route.Builder[ServerContext, IO]
  *    )(using getRequest: GetCurrentRequest[IO]): FrameworkServerRouter.Routes[ServerContext, IO] = {
  *      FrameworkServerRouter.Routes(
  *        Vector(
  *          builder.endpoint.regular(Endpoints.Users.me)(
  *            _
  *              .serverSecurityLogicPure(app.auth.Authenticator.authenticated)
  *              .serverLogicSuccess(app.endpoints.UsersMe.apply)
  *          ),
  *          builder.endpoint.sseWithWs(Endpoints.AppUpdates.unauthenticated)(
  *            toSSE = _.toSSEJson[IO](sseKeepAlive),
  *            build = _.serverLogicSuccessPure(app.endpoints.AppUpdates.forUnauthenticated),
  *          ),
  *        )
  *      )
  *    }
  * }
  * }}}
  */
trait ServerRouter[Context] {
  export org.http4s.ContextRoutes
  export sttp.tapir.server.http4s.{Http4sServerInterpreter, RichHttp4sEndpoint}

  /** Creates routes that depend on the [[Context]]. */
  def routes(builder: ServerRouter.Route.Builder[Context, IO])(using
    getRequest: GetCurrentRequest[IO]
  ): ServerRouter.Routes[Context, IO]
}
object ServerRouter {
  case class Route[Context, F[_]](
    regular: Option[ContextRoutes[Context, F]],
    webSocket: Option[WebSocketBuilder2[F] => ContextRoutes[Context, F]],
  )
  object Route {
    def both[Context, F[_]](
      regular: ContextRoutes[Context, F],
      webSocket: WebSocketBuilder2[F] => ContextRoutes[Context, F],
    ): Route[Context, F] =
      apply(Some(regular), Some(webSocket))

    def regular[Context, F[_]](regular: ContextRoutes[Context, F]): Route[Context, F] =
      apply(Some(regular), webSocket = None)

    def webSocket[Context, F[_]](webSocket: WebSocketBuilder2[F] => ContextRoutes[Context, F]): Route[Context, F] =
      apply(regular = None, Some(webSocket))

    def builderFor[Context, F[_]](interpreter: Http4sServerInterpreter[F])(using
      ClassTag[Context]
    ): Builder[Context, F] = Builder(interpreter)

    class Builder[Context, F[_]](val serverInterpreter: Http4sServerInterpreter[F])(using ClassTag[Context]) { self =>

      /** Creates a regular route. */
      def regular(se: ServerEndpoint[Fs2Streams[F] & TapirHttp4sContext[Context], F]): Route[Context, F] =
        Route.regular(serverInterpreter.toContextRoutes(se))

      /** Creates a websocket route. */
      def webSocket(
        se: ServerEndpoint[Fs2Streams[F] & TapirHttp4sContext[Context] & WebSockets, F]
      ): Route[Context, F] =
        Route.webSocket(serverInterpreter.toContextWebSocketRoutes(se))

      /** Creates a [[Route]] from an endpoint. */
      object endpoint {

        /** Creates a regular route. */
        def regular[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R](
          endpoint: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]
        )(using concat: ParamConcat[INPUT, Context])(
          build: Endpoint[
            SECURITY_INPUT,
            concat.Out,
            ERROR_OUTPUT,
            OUTPUT,
            R & TapirHttp4sContext[Context],
          ] => ServerEndpoint[Fs2Streams[F] & TapirHttp4sContext[Context], F]
        ): Route[Context, F] =
          self.regular(build(endpoint.contextIn[Context]()))

        /** Creates a websocket route. */
        def webSocket[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R](
          endpoint: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]
        )(using inputConcat: ParamConcat[INPUT, Context])(
          build: Endpoint[
            SECURITY_INPUT,
            inputConcat.Out,
            ERROR_OUTPUT,
            OUTPUT,
            R & TapirHttp4sContext[Context],
          ] => ServerEndpoint[Fs2Streams[F] & TapirHttp4sContext[Context] & WebSockets, F]
        ): Route[Context, F] =
          self.webSocket(build(endpoint.contextIn[Context]()))

        /** Creates a server-sent events route with a websocket alternative.
          *
          * Example:
          * ```
          * builder.endpoint.sseWithWs(Endpoints.AppUpdates.unauthenticated)(
          *   toSSE = _.toSSEJson[IO](sseKeepAlive),
          *   build = _.serverLogicSuccessPure(app.endpoints.AppUpdates.forUnauthenticated),
          * )
          * ```
          */
        def sseWithWs[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R](
          endpoint: EndpointSSEWithWS[F, SECURITY_INPUT, INPUT, OUTPUT, ERROR_OUTPUT, R]
        )(using inputConcat: ParamConcat[INPUT, Context])[PRINCIPAL](
          toSSE: Endpoint[
            SECURITY_INPUT,
            inputConcat.Out,
            ERROR_OUTPUT,
            OUTPUT,
            R & ServerSentEvents & TapirHttp4sContext[Context],
          ] => Endpoint[
            SECURITY_INPUT,
            inputConcat.Out,
            ERROR_OUTPUT,
            Stream[F, OUTPUT],
            R & Fs2Streams[F] & TapirHttp4sContext[Context],
          ],
          build: Endpoint[
            SECURITY_INPUT,
            inputConcat.Out,
            ERROR_OUTPUT,
            Stream[F, OUTPUT],
            R & Fs2Streams[F] & TapirHttp4sContext[Context],
          ] => ServerEndpoint[Fs2Streams[F] & TapirHttp4sContext[Context], F],
        ): Route[Context, F] = {
          Route.both(
            regular = endpoint.sse.contextIn[Context]().pipe(toSSE).pipe(build).pipe(serverInterpreter.toContextRoutes),
            webSocket = endpoint.webSocketAsSSE
              .contextIn[Context]()
              .relaxRequirements[R & Fs2Streams[F] & TapirHttp4sContext[Context]]
              .pipe(build)
              .pipe(serverInterpreter.toContextWebSocketRoutes),
          )
        }
      }
    }
  }

  case class Routes[Context, F[_]](
    routes: Vector[Route[Context, F]]
  ) {

    /** Returns [[None]] if there are no regular routes. */
    def regular(using Monad[F]): Option[ContextRoutes[Context, F]] =
      NonEmptyVector.fromVector(routes.flatMap(_.regular)).map(_.reduceK)

    /** Returns [[None]] if there are no web socket routes. */
    def webSocket(using Monad[F]): Option[WebSocketBuilder2[F] => ContextRoutes[Context, F]] =
      routes.flatMap(_.webSocket).reduceLeftOption { (f, g) => builder => f(builder) <+> g(builder) }
  }
}
