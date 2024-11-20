package framework.config

import com.comcast.ip4s.*
import org.http4s.Uri
import cats.syntax.all.*

case class HttpServerConfig(host: Host, port: Port)
object HttpServerConfig {
  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, HttpServerConfig] = (
    ciris.env(prefix("HTTP_HOST")).as[Host].default(host"127.0.0.1"),
    ciris.env(prefix("HTTP_PORT")).as[Port].default(port"3005"),
  ).parMapN(apply)
}

/** @param corsAllowedDomains
  *   the domains that are allowed to make CORS requests
  */
case class HttpConfig(
  server: HttpServerConfig,
  frontendUri: FrontendUri,
  corsAllowedDomains: Set[Host] = Set.empty,
) {
  lazy val corsAllowedDomainsStrings: Set[String] = corsAllowedDomains.iterator.map(_.show).toSet
}
object HttpConfig {
  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, HttpConfig] = (
    HttpServerConfig.cirisConfig,
    ciris.env(prefix("HTTP_FRONTEND_URI")).as[FrontendUri],
    ciris.default(Set.empty), // Do not read this from the environment, as we currently do not need it.
  ).parMapN(apply)
}
