package framework.utils

/** Helper trait for creating routes from endpoints.
  *
  * Example:
  * {{{
  * class ServerRouter(using Transactor[IO]) extends framework.utils.ServerRouter[ServerContext] {
  *    override def routesVector(
  *      serverInterpreter: Http4sServerInterpreter[IO]
  *    ): NonEmptyVector[ContextRoutes[ServerContext, IO]] = {
  *      NonEmptyVector.of(
  *        serverInterpreter.toContextRoutes(
  *          Endpoints.Users.me
  *            .contextIn[ServerContext]()
  *            .serverSecurityLogicPure(app.auth.Authenticator.authenticated)
  *            .serverLogicSuccess(app.endpoints.UsersMe.apply)
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
  def routesVector(serverInterpreter: Http4sServerInterpreter[IO]): NonEmptyVector[ContextRoutes[Context, IO]]

  /** Creates routes that depend on the [[Context]]. */
  def routes(serverInterpreter: Http4sServerInterpreter[IO]): ContextRoutes[Context, IO] =
    routesVector(serverInterpreter).reduceK
}
