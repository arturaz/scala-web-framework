package framework.config

import cats.effect.kernel.{Async, Resource}
import cats.syntax.parallel.*
import cats.syntax.show.*
import ciris.http4s.*
import ciris.{ConfigValue, Secret}
import com.comcast.ip4s.*
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import doobie.hikari.HikariTransactor
import doobie.otel4s.TracedTransactor
import doobie.util.log.LogHandler
import doobie.util.transactor.Transactor
import fly4s.Fly4s
import fly4s.data.Fly4sConfig
import org.typelevel.otel4s.trace.{Tracer, TracerProvider}
import fly4s.data.ValidatePattern
import org.tpolecat.typename.TypeName

case class PostgresqlConfig(
  username: String = "postgres",
  password: Secret[String],
  database: String,
  host: Host = host"localhost",
  port: Port = port"5432",
  sslMode: Option[PostgresqlConfig.SslMode] = None,
) {
  def jdbcUrl: String =
    s"jdbc:postgresql://${host.show}:${port.show}/${database.show}?${sslMode.fold("")(sslMode => s"sslmode=${sslMode.asString}")}"

  def defaultFly4sConfig: Fly4sConfig = Fly4sConfig(ignoreMigrationPatterns =
    List(
      // Ignore pending migrations when validating the migrations, as that does not make sense. We will apply
      // pending migrations later.
      ValidatePattern.ignorePendingMigrations
    )
  )

  /** Returns the [[Resource]] that creates a Fly4s client. */
  def flywayResource(
    config: Fly4sConfig = defaultFly4sConfig,
    classLoader: ClassLoader = Thread.currentThread.getContextClassLoader,
  ): Resource[IO, Fly4s[IO]] = Fly4s.make[IO](
    url = jdbcUrl,
    user = Some(username),
    password = Some(password.value.toCharArray),
    config = config,
    classLoader = classLoader,
  )

  /** Returns the [[Resource]] that creates a traced Hikari-based transactor. */
  def transactorResource[F[_]: Async](
    logHandler: Option[LogHandler[F]],
    modConfig: HikariConfig => HikariConfig = identity,
    tracerConfig: TracedTransactor.Config[F] = TracedTransactor.Config.default[F],
  )(using Tracer[F]): Resource[F, Transactor[F]] =
    for {
      hikari <- HikariTransactor.fromHikariConfig[F](
        {
          // For the full list of hikari configurations see
          // https://github.com/brettwooldridge/HikariCP#gear-configuration-knobs-baby
          val config = new HikariConfig()
          config.setDriverClassName(PostgresqlConfig.JDBCDriverName)
          config.setJdbcUrl(jdbcUrl)
          config.setUsername(username)
          config.setPassword(password.value)
          modConfig(config)
        },
        logHandler,
      )
    } yield TracedTransactor(hikari, logHandler.getOrElse(LogHandler.noop), tracerConfig)
}
object PostgresqlConfig {
  enum SslMode(val asString: String) {
    case Require extends SslMode("require")
    case VerifyCa extends SslMode("verify-ca")
    case VerifyFull extends SslMode("verify-full")
  }
  object SslMode {
    given ConfigDecoder[String, SslMode] =
      summon[ConfigDecoder[String, String]].mapOption(summon[TypeName[SslMode]].value) {
        case SslMode.Require.asString    => Some(SslMode.Require)
        case SslMode.VerifyCa.asString   => Some(SslMode.VerifyCa)
        case SslMode.VerifyFull.asString => Some(SslMode.VerifyFull)
        case _                           => None
      }
  }

  /** The class name of the JDBC driver. */
  final val JDBCDriverName: String = "org.postgresql.Driver"

  /** Loads the PostgreSQL JDBC driver. */
  def loadJDBCPostgresqlDriver: IO[Unit] =
    IO.blocking(Class.forName(JDBCDriverName)).void

  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, PostgresqlConfig] = (
    ciris.env(prefix("POSTGRESQL_USER")).as[String].default("postgres"),
    ciris.env(prefix("POSTGRESQL_PASSWORD")).as[String].secret,
    ciris.env(prefix("POSTGRESQL_DATABASE")).as[String],
    ciris.env(prefix("POSTGRESQL_HOST")).as[Host].default(host"localhost"),
    ciris.env(prefix("POSTGRESQL_PORT")).as[Port].default(port"5432"),
    ciris.env(prefix("POSTGRESQL_SSL_MODE")).as[SslMode].option,
  ).parMapN(apply)
}
