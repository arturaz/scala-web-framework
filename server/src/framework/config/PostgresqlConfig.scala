package framework.config

import cats.effect.kernel.Resource
import cats.syntax.show.*
import com.comcast.ip4s.*
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import doobie.hikari.HikariTransactor
import doobie.util.transactor.Transactor
import fly4s.Fly4s
import fly4s.data.Fly4sConfig
import doobie.util.log.LogHandler
import cats.effect.kernel.Async

case class PostgresqlConfig(
  username: String = "postgres",
  password: String,
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
  ) = Fly4s.make[IO](
    url = jdbcUrl,
    user = Some(username),
    password = Some(password.toCharArray),
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
        config.setPassword(password)
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
    IO(Class.forName(JDBCDriverName)).void
}
