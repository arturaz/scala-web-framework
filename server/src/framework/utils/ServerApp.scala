package framework.utils

import cats.effect.{ExitCode, IOApp}
import com.zaxxer.hikari.HikariDataSource
import fly4s.Fly4s
import fly4s.data.{Fly4sConfig, ValidatePattern}
import framework.config.{IsProductionMode, PostgresqlConfig}
import framework.db.{*, given}
import java.nio.file.Paths
import java.nio.file.Files
import java.nio.charset.StandardCharsets

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

  /** Runs the server. */
  def run(cfg: ServerAppConfig, args: List[String]): IO[ExitCode]

  override def run(args: List[String]): IO[ExitCode] = {
    val appConfigValue = for {
      isProduction <- IsProductionMode.cirisConfig[IO]
      cfg <- if (isProduction) productionAppConfig else ConfigValue.default(developmentAppConfig)
    } yield (cfg, isProduction)

    for {
      (cfg, isProduction) <- appConfigValue.load
      _ <- applyLoggingDefaults(isProduction).to[IO]
      result <- builtInCommands(cfg, args).getOrElse(run(cfg, args))
    } yield result
  }

  /** Handles some built-in commands.
    *
    * @return
    *   `Some` if the command was handled, `None` if not.
    */
  def builtInCommands(cfg: ServerAppConfig, args: List[String]): Option[IO[ExitCode]] = args match {
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

  def runMigrations(cfg: ServerAppConfig)(using Transactor[IO]): IO[ExitCode] =
    framework.db
      .runDbMigrations(flywayResource(postgresqlConfig(cfg)), recreateSchemaAndRetryOnFailure = !isProduction(cfg), log)
      .flatMap {
        case true  => IO.pure(ExitCode.Success)
        case false => IO.pure(ExitCode.Error)
      }

  /** Obtains the resource for the database connection, which is also responsible for running migrations upon connection
    * if `migrateOnConnect` is true.
    *
    * When the server starts you usually want to migrate the database, that's why `migrateOnConnect` is true by default.
    */
  def postgresqlResource(
    cfg: ServerAppConfig,
    migrateOnConnect: Boolean = true,
  ): Resource[IO, Transactor[IO] { type A = HikariDataSource }] =
    postgresqlConfig(cfg)
      .transactorResource[IO](logHandler = Some(new FrameworkDoobieLogHandler(log)))
      .evalMap(transactor =>
        if (migrateOnConnect) runMigrations(cfg)(using transactor).flatMap(_.successfulOrExit[IO].as(transactor))
        else IO.pure(transactor)
      )
}
