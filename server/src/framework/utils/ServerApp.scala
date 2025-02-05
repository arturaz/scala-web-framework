package framework.utils

import cats.effect.{ExitCode, IOApp}
import com.zaxxer.hikari.HikariDataSource
import fly4s.Fly4s
import fly4s.data.{Fly4sConfig, ValidatePattern}
import framework.config.{IsProductionMode, PostgresqlConfig}
import framework.db.{*, given}
import io.opentelemetry.api.OpenTelemetry as JOpenTelemetry
import io.opentelemetry.instrumentation.runtimemetrics.java17.RuntimeMetrics
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.instrumentation.ce.IORuntimeMetrics
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.oteljava.OtelJava
import org.typelevel.otel4s.trace.{Tracer, TracerProvider}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/** Boilerplate for a server application. */
trait ServerApp extends IOApp {

  /** Type of the configuration for the server. */
  type ServerAppConfig

  /** The prefix for the environment variables that Ciris configuration library reads. */
  given envConfigPrefix: EnvConfigPrefix

  /** The default configuration for the server in development mode. */
  def developmentAppConfig: ServerAppConfig

  /** The default configuration for the server in production mode. */
  def productionAppConfig: ConfigValue[IO, ServerAppConfig]

  def postgresqlConfig(cfg: ServerAppConfig): PostgresqlConfig

  def isProduction(cfg: ServerAppConfig): IsProductionMode

  /** The name of this server. */
  def serverName: IO[String]

  /** The version of the code running in the server. */
  def serverVersion: IO[String]

  /** Runs the server. */
  def run(cfg: ServerAppConfig, args: List[String])(using
    Tracer[IO],
    TracerProvider[IO],
    MeterProvider[IO],
  ): IO[ExitCode]

  override def run(args: List[String]): IO[ExitCode] = {
    val appConfigValue = for {
      isProduction <- IsProductionMode.cirisConfig[IO]
      cfg <- if (isProduction) productionAppConfig else ConfigValue.default(developmentAppConfig)
    } yield (cfg, isProduction)

    val resource = for {
      (cfg, isProduction) <- appConfigValue.load.toResource
      _ <- applyLoggingDefaults(isProduction).to[IO].toResource
      serverName <- this.serverName.toResource
      serverVersion <- this.serverVersion.toResource
      otel4s <- OtelJava.autoConfigured[IO]()
      tracer <- otel4s.tracerProvider.tracer(serverName).withVersion(serverVersion).get.toResource
      given TracerProvider[IO] = otel4s.tracerProvider
      given MeterProvider[IO] = otel4s.meterProvider
      given Tracer[IO] = tracer
      _ <- registerJavaRuntimeMetrics(otel4s.underlying)
      _ <- registerCatsEffectMetrics
      result <- builtInCommands(cfg, args)
        .map(tracer.span("startup: built-in-commands").surround)
        .getOrElse(run(cfg, args))
        .toResource
    } yield result

    resource.use(IO.pure)
  }

  /** https://typelevel.org/otel4s/oteljava/metrics-jvm-runtime.html#java-17-and-newer */
  def registerJavaRuntimeMetrics(openTelemetry: JOpenTelemetry): Resource[IO, RuntimeMetrics] = for {
    _ <- Resource.eval(log.info("Registering JVM runtime metrics to OpenTelemetry..."))
    metrics <- Resource.fromAutoCloseable(IO(RuntimeMetrics.create(openTelemetry)))
    _ <- Resource.eval(log.info("Registered JVM runtime metrics to OpenTelemetry."))
  } yield metrics

  /** https://typelevel.org/otel4s/instrumentation/metrics-cats-effect-io-runtime.html#registering-metrics-collectors */
  def registerCatsEffectMetrics(using MeterProvider[IO]): Resource[IO, Unit] = for {
    _ <- Resource.eval(log.info("Registering Cats Effect metrics to OpenTelemetry..."))
    _ <- IORuntimeMetrics.register[IO](runtime.metrics, IORuntimeMetrics.Config.default)
    _ <- Resource.eval(log.info("Registered Cats Effect metrics to OpenTelemetry."))
  } yield ()

  /** Handles some built-in commands.
    *
    * @return
    *   `Some` if the command was handled, `None` if not.
    */
  def builtInCommands(cfg: ServerAppConfig, args: List[String])(using Tracer[IO]): Option[IO[ExitCode]] =
    args match {
      case "db-migrate" :: Nil =>
        Some(postgresqlResource(cfg, migrateOnConnect = false).use(implicit xa => runMigrations(cfg)))

      case "db-dump" :: pathStr :: Nil =>
        Some(for {
          path <- IO(Paths.get(pathStr))
          sql <- postgresqlResource(cfg).use(implicit xa => framework.utils.db.DumpData.dataDumps())
          _ <- IO.blocking(Files.writeString(path, sql, StandardCharsets.UTF_8))
        } yield ExitCode.Success)

      case _ => None
    }

  // Helpers

  def flywayResource(cfg: PostgresqlConfig): Resource[IO, Fly4s[IO]] = cfg.flywayResource(
    config = Fly4sConfig(ignoreMigrationPatterns = List(ValidatePattern.ignorePendingMigrations))
  )

  def runMigrations(cfg: ServerAppConfig)(using transactor: Transactor[IO], tracer: Tracer[IO]): IO[ExitCode] = {
    val isProduction = this.isProduction(cfg)
    tracer
      .span("database-migration", isProduction.toOtelAttribute)
      .surround(
        framework.db
          .runDbMigrations(flywayResource(postgresqlConfig(cfg)), recreateSchemaAndRetryOnFailure = !isProduction, log)
          .flatMap {
            case true  => IO.pure(ExitCode.Success)
            case false => IO.pure(ExitCode.Error)
          }
      )
  }

  /** Obtains the resource for the database connection, which is also responsible for running migrations upon connection
    * if `migrateOnConnect` is true.
    *
    * When the server starts you usually want to migrate the database, that's why `migrateOnConnect` is true by default.
    */
  def postgresqlResource(
    cfg: ServerAppConfig,
    migrateOnConnect: Boolean = true,
  )(using Tracer[IO]): Resource[IO, Transactor[IO]] =
    postgresqlConfig(cfg)
      .transactorResource[IO](logHandler = Some(new FrameworkDoobieLogHandler(log)))
      .evalMap(implicit transactor =>
        if (migrateOnConnect) runMigrations(cfg).flatMap(_.successfulOrExit[IO].as(transactor))
        else IO.pure(transactor)
      )
}
