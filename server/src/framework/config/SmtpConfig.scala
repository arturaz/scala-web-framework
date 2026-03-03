package framework.config

import cats.effect.Async
import ciris.ConfigError
import com.comcast.ip4s.{Host, Port, SocketAddress}
import fs2.io.net.Network
import fs2.io.net.tls.{TLSContext, TLSParameters}
import org.typelevel.log4cats.Logger
import pencilmail.data.{Credentials, PasswordType, UsernameType}
import pencilmail.{Client, SmtpMode, SocketTimeoutConfig}

import scala.util.Try

/** SMTP configuration. */
case class SmtpConfig(
  host: Host,
  port: Port,
  mode: SmtpMode,
  credentials: Option[Credentials],
) {
  def socketAddress: SocketAddress[Host] = SocketAddress(host, port)

  /**
   * @param tls obtain with `fs2.io.net.Network[F].tlsContext`
   * @param logger You probably want to use [[framework.utils.ScribeLog4CatsLogger]] for this.
   **/
  def toClient[F[_]: {Async, Network}](
    tls: TLSContext[F],
    logger: Logger[F],
    tlsParameters: TLSParameters = TLSParameters.Default,
    socketTimeoutConfig: SocketTimeoutConfig = SocketTimeoutConfig.default,
  ): Client[F] = Client[F](
    socketAddress,
    mode,
    credentials,
    tls,
    logger,
    tlsParameters,
    socketTimeoutConfig,
  )
}
object SmtpConfig {
  given cirisConfigDecoderSmtpMode: ConfigDecoder[String, SmtpMode] =
    ConfigDecoder.instance {
      case (_, "plain") => Right(SmtpMode.Plain)
      case (_, "starttls") => Right(SmtpMode.StartTLS)
      case (_, "tls") => Right(SmtpMode.TLS)
      case (key, str) => Left(ConfigError.decode(typeName = "SmtpMode", key = key, value = str))
    }

  given cirisConfigDecoderUsername: ConfigDecoder[String, UsernameType.Username] = 
    ConfigDecoder[String].mapEither((key, str) =>
      Try(UsernameType.Username(str)).toEither.left.map(err => ConfigError(err.getMessage))
    )
  
  given cirisConfigDecoderPassword: ConfigDecoder[String, PasswordType.Password] = 
    ConfigDecoder[String].mapEither((key, str) =>
      Try(PasswordType.Password(str)).toEither.left.map(err => ConfigError(err.getMessage))
    )

  given cirisConfigCredentials[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, Option[Credentials]] = (
    ciris.env(prefix("SMTP_USER")).as[UsernameType.Username],
    ciris.env(prefix("SMTP_PASSWORD")).as[PasswordType.Password].secret,
  ).mapN ( (user, password) => Credentials(user, password.value) ).option

  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, SmtpConfig] = (
    ciris.env(prefix("SMTP_HOST")).as[Host],
    ciris.env(prefix("SMTP_PORT")).as[Port],
    ciris.env(prefix("SMTP_MODE")).as[SmtpMode],
    cirisConfigCredentials,
  ).mapN(apply)
}