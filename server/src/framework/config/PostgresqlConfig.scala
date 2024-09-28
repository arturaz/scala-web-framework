package framework.config

import cats.effect.kernel.{Async, Resource}
import cats.syntax.parallel.*
import cats.syntax.show.*
import ciris.http4s.*
import ciris.{ConfigValue, Secret}
import com.comcast.ip4s.*
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import doobie.hikari.HikariTransactor
import doobie.util.log.LogHandler
import doobie.util.transactor.Transactor
import fly4s.Fly4s
import fly4s.data.Fly4sConfig

case class PostgresqlConfig(
  username: String = "postgres",
  password: Secret[String],
  database: String,
  host: Host = host"localhost",
  port: Port = port"5432",
) {
  def jdbcUrl: String =
    s"jdbc:postgresql://${host.show}:${port.show}/${database.show}"

  /** Returns the [[Resource]] that creates a Fly4s client. */
  def flywayResource(
    config: Fly4sConfig = Fly4sConfig.default,
    classLoader: ClassLoader = Thread.currentThread.getContextClassLoader,
  ): Resource[IO, Fly4s[IO]] = Fly4s.make[IO](
    url = jdbcUrl,
    user = Some(username),
    password = Some(password.value.toCharArray),
    config = config,
    classLoader = classLoader,
  )

  /** Returns the [[Resource]] that creates a Hikari-based transactor. */
  def transactorResource[F[_]: Async](
    logHandler: Option[LogHandler[F]],
    modConfig: HikariConfig => HikariConfig = identity,
  ): Resource[F, Transactor[F] { type A = HikariDataSource }] =
    HikariTransactor.fromHikariConfig[F](
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
}
object PostgresqlConfig {

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
  ).parMapN(apply)
}
