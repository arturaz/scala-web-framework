package framework.config

import cats.syntax.all.*
import com.comcast.ip4s.*
import org.http4s.Uri
import org.http4s.server.middleware.{CORS, CORSPolicy}
import scribe.Level
import org.http4s.server.middleware.Logger
import org.typelevel.ci.CIString
import scribe.mdc.MDC
import scala.concurrent.duration.*
import java.nio.file.Path

/** TLS configuration for the HTTP server.
  *
  * @param keyStoreFile
  *   path to the JKS keystore file
  * @param keyStorePassword
  *   password for the keystore
  * @param keyPassword
  *   password for the key
  */
case class HttpServerTLSConfig(
  keyStoreFile: Path,
  keyStorePassword: String,
  keyPassword: String,
)
object HttpServerTLSConfig {
  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, HttpServerTLSConfig] = (
    ciris.env(prefix("HTTP_TLS_KEYSTORE_FILE")).as[Path],
    ciris.env(prefix("HTTP_TLS_KEYSTORE_PASSWORD")).as[String],
    ciris.env(prefix("HTTP_TLS_KEY_PASSWORD")).as[String],
  ).mapN(apply)
}

case class HttpServerConfig(host: Host, port: Port, tls: Option[HttpServerTLSConfig])
object HttpServerConfig {
  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, HttpServerConfig] = (
    ciris.env(prefix("HTTP_HOST")).as[Host].default(host"127.0.0.1"),
    ciris.env(prefix("HTTP_PORT")).as[Port].default(port"3005"),
    ciris.env(prefix("HTTP_TLS")).as[Boolean].default(false).flatMap {
      case false => ConfigValue.default(None)
      case true  => HttpServerTLSConfig.cirisConfig.map(Some(_))
    },
  ).mapN(apply)
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
  ).mapN(apply)
}

case class ClientRequestTracingConfig(maxDriftFromCurrentTime: FiniteDuration)
object ClientRequestTracingConfig {
  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, ClientRequestTracingConfig] =
    ciris
      .env(prefix("CLIENT_REQUEST_TRACING_MAX_DRIFT_FROM_CURRENT_TIME"))
      .as[FiniteDuration]
      .default(30.seconds)
      .map(apply)
}

/** @param corsAllowedDomains
  *   the domains that are allowed to make CORS requests
  */
case class HttpConfig(
  server: HttpServerConfig,
  logging: HttpServerLoggingConfig,
  clientRequestTracing: ClientRequestTracingConfig,
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
    ClientRequestTracingConfig.cirisConfig,
    ciris.env(prefix("HTTP_FRONTEND_URI")).as[FrontendUri],
    ciris.default(Set.empty), // Do not read this from the environment, as we currently do not need it.
  ).mapN(apply)
}
