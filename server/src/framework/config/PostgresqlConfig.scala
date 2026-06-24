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
import org.flywaydb.core.api.callback.Callback
import org.flywaydb.core.api.callback.Context
import org.flywaydb.core.api.callback.Event
import scala.jdk.CollectionConverters.*
import scala.concurrent.duration.FiniteDuration

/** @param idleTimeout
  *   maximum amount of time (in milliseconds) that a connection is allowed to sit idle in the pool. Whether a
  *   connection is retired as idle or not is subject to a maximum variation of +30 seconds, and average variation of
  *   +15 seconds. A connection will never be retired as idle before this timeout. A value of 0 means that idle
  *   connections are never removed from the pool. Uses default value if not set.
  * @param minIdleConnections
  *   minimum number of idle connections that HikariCP tries to maintain in the pool, including both idle and in-use
  *   connections. If the idle connections dip below this value, HikariCP will make a best effort to restore them
  *   quickly and efficiently. Uses default value if not set.
  * @param maxPoolSize
  *   maximum size that the pool is allowed to reach, including both idle and in-use connections. Basically this value
  *   will determine the maximum number of actual connections to the database backend. When the pool reaches this size,
  *   and no idle connections are available, calls to getConnection() will block for up to connectionTimeout
  *   milliseconds before timing out. Uses default value if not set.
  */
case class PostgresqlConfig(
  username: String = "postgres",
  password: Secret[String],
  database: String,
  host: Host = host"localhost",
  port: Port = port"5432",
  sslMode: Option[PostgresqlConfig.SslMode] = None,
  idleTimeout: Option[FiniteDuration] = None,
  minIdleConnections: Option[Int] = None,
  maxPoolSize: Option[Int] = None,
) {
  def jdbcUrl: String =
    show"jdbc:postgresql://$host:$port/$database?${sslMode.fold("")(sslMode => show"sslmode=${sslMode.asString}")}"

  def defaultFly4sConfig: Fly4sConfig = Fly4sConfig(
    ignoreMigrationPatterns = List(
      // Ignore pending migrations when validating the migrations, as that does not make sense. We will apply
      // pending migrations later.
      ValidatePattern.ignorePendingMigrations
    ),
    callbacks = List(ErrorDebugCallback),
  )

  /** Logs the error message for a migration error. */
  object ErrorDebugCallback extends Callback {
    given CanEqual[Event, Event] = CanEqual.derived

    override def getCallbackName(): String = "ErrorDebugCallback"

    override def handle(event: Event, context: Context): Unit = {
      if (event != Event.AFTER_EACH_MIGRATE_STATEMENT_ERROR) return

      val statement = context.getStatement()
      val migration = context.getMigrationInfo()

      appLogger.error(
        show"""|Error applying migration (${migration.getType().toString()}) '${migration.getScript()}'
               |
               |Statement: 
               |${statement.getSql().indentLines(2)}
               |
               |Warnings:
               |${statement.getWarnings().asScala.mkString("\n").nonEmptyOption.getOrElse("<none>").indentLines(2)}
               |
               |Errors:
               |${statement.getErrors().asScala.mkString("\n").nonEmptyOption.getOrElse("<none>").indentLines(2)}
               |""".stripMargin
      )
    }

    override def canHandleInTransaction(event: Event, context: Context): Boolean = true

    override def supports(event: Event, context: Context): Boolean =
      event == Event.AFTER_EACH_MIGRATE_STATEMENT_ERROR
  }

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
          idleTimeout.foreach(duration => config.setIdleTimeout(duration.toMillis))
          minIdleConnections.foreach(config.setMinimumIdle)
          maxPoolSize.foreach(config.setMaximumPoolSize)
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
    ciris.env(prefix("POSTGRESQL_IDLE_TIMEOUT")).as[FiniteDuration].option,
    ciris.env(prefix("POSTGRESQL_MIN_IDLE_CONNECTIONS")).as[Int].option,
    ciris.env(prefix("POSTGRESQL_MAX_POOL_SIZE")).as[Int].option,
  ).parMapN(apply)
}
