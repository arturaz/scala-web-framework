package framework.config

import cats.effect.kernel.Resource
import cats.syntax.show.*
import com.comcast.ip4s.*
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import doobie.hikari.HikariTransactor
import doobie.util.transactor.Transactor
import fly4s.Fly4s
import fly4s.data.Fly4sConfig

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
  def transactorResource: Resource[IO, Transactor[IO] { type A = HikariDataSource }] =
    HikariTransactor.fromHikariConfig[IO] {
      // For the full list of hikari configurations see
      // https://github.com/brettwooldridge/HikariCP#gear-configuration-knobs-baby
      val config = new HikariConfig()
      config.setDriverClassName(PostgresqlConfig.JDBCDriverName)
      config.setJdbcUrl(jdbcUrl)
      config.setUsername(username)
      config.setPassword(password)
      config
    }
}
object PostgresqlConfig {

  /** The class name of the JDBC driver. */
  final val JDBCDriverName: String = "org.postgresql.Driver"

  /** Loads the PostgreSQL JDBC driver. */
  def loadJDBCPostgresqlDriver: IO[Unit] =
    IO(Class.forName(JDBCDriverName)).void
}
