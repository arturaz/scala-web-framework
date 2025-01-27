package framework.config

import cats.syntax.all.*
import com.comcast.ip4s.*
import org.http4s.Uri
import org.http4s.server.middleware.{CORS, CORSPolicy}
import scribe.Level
import org.http4s.server.middleware.Logger
import org.typelevel.ci.CIString
import scribe.mdc.MDC

case class HttpServerConfig(host: Host, port: Port)
object HttpServerConfig {
  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, HttpServerConfig] = (
    ciris.env(prefix("HTTP_HOST")).as[Host].default(host"127.0.0.1"),
    ciris.env(prefix("HTTP_PORT")).as[Port].default(port"3005"),
  ).parMapN(apply)
}

case class HttpServerLoggingConfig(logHeaders: Boolean, logBody: Boolean, logLevel: Level) {

  /** Example:
    * {{{
    * val router = Router(...)
    * router.pipe(cfg.logging.logger()(_))
    * }}}
    */
  def logger(redactHeadersWhen: CIString => Boolean = Logger.defaultRedactHeadersWhen) =
    Logger.httpRoutes[IO](
      logHeaders = logHeaders,
      logBody = logBody,
      redactHeadersWhen = redactHeadersWhen,
      logAction = Some(log.log(logLevel, mdc = summon, _)),
    )
}
object HttpServerLoggingConfig {
  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, HttpServerLoggingConfig] = (
    ciris.env(prefix("HTTP_LOG_HEADERS")).as[Boolean].default(true),
    ciris.env(prefix("HTTP_LOG_BODY")).as[Boolean].default(false),
    ciris.env(prefix("HTTP_LOG_LEVEL")).as[Level].default(Level.Info),
  ).parMapN(apply)
}

/** @param corsAllowedDomains
  *   the domains that are allowed to make CORS requests
  */
case class HttpConfig(
  server: HttpServerConfig,
  logging: HttpServerLoggingConfig,
  frontendUri: FrontendUri,
  corsAllowedDomains: Set[Host] = Set.empty,
) {
  lazy val corsAllowedDomainsStrings: Set[String] = corsAllowedDomains.iterator.map(_.show).toSet

  def corsPolicy: CORSPolicy =
    CORS.policy
      // Allow sending the HTTP cookies for CORS requests.
      // https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS/Errors/CORSMIssingAllowCredentials
      .withAllowCredentials(true)
      .withAllowOriginHost { host =>
        corsAllowedDomainsStrings.contains(host.host.show)
      }
}
object HttpConfig {
  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, HttpConfig] = (
    HttpServerConfig.cirisConfig,
    HttpServerLoggingConfig.cirisConfig,
    ciris.env(prefix("HTTP_FRONTEND_URI")).as[FrontendUri],
    ciris.default(Set.empty), // Do not read this from the environment, as we currently do not need it.
  ).parMapN(apply)
}
