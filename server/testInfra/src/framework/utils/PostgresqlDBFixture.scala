package framework.utils

import cats.effect.{IO, Resource, SyncIO}
import cats.syntax.all.*
import fly4s.data.Fly4sConfig
import framework.config.PostgresqlConfig
import framework.exts.*
import framework.prelude.*
import munit.*
import org.typelevel.otel4s.trace.Tracer

import java.sql.{Connection, DriverManager}

/** Gives you access to PostgreSQL database, migrations are run against it.
  *
  * You should use [[dbResourceCached]] in your code.
  *
  * @param postgresqlConfig
  *   The config of the PostgreSQL database for the tests.
  * @param classloaderForMigrations
  *   The classloader to use for migrations.
  */
trait WithPostgresqlDB(
  val postgresqlConfig: PostgresqlConfig,
  val classloaderForMigrations: ClassLoader = Thread.currentThread.getContextClassLoader,
)(using tracer: Tracer[IO])
    extends WithDatabase.External {

  def fly4sConfig: Fly4sConfig = postgresqlConfig.defaultFly4sConfig

  override def dbResource: Resource[IO, Transactor[IO]] =
    for {
      transactor <- postgresqlConfig.transactorResource[IO](logHandler = Some(new FrameworkDoobieLogHandler(log)))
      _ <- Resource.eval(
        framework.db.runDbMigrationsUsingResource(
          postgresqlConfig.flywayResource(fly4sConfig, classloaderForMigrations),
          recreateSchemaAndRetryOnFailure = true,
          log,
        )(using transactor = transactor)
      )
    } yield transactor
}
