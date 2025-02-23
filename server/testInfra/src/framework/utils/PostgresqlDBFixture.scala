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

/** Gives you access to PostgreSQL database, migrations are run against it. */
trait PostgresqlDBFixture extends DBFixture { self: CatsEffectSuite & FrameworkTestSuiteHelpers =>

  /** The config of the PostgreSQL database for the tests. */
  def postgresqlConfig: PostgresqlConfig

  override def connectionResource: Resource[IO, Connection] =
    Resource.fromAutoCloseable(IO.blocking {
      val config = postgresqlConfig
      // Load the driver
      val _ = Class.forName("org.postgresql.Driver")
      DriverManager.getConnection(config.jdbcUrl, config.username, config.password.value)
    })

  def fly4sConfig: Fly4sConfig = postgresqlConfig.defaultFly4sConfig

  def classloaderForMigrations: ClassLoader = Thread.currentThread.getContextClassLoader

  override def dbResource: Resource[IO, Transactor.Aux[IO, Connection]] =
    for {
      transactor <- super.dbResource
      _ <- Resource.eval(
        framework.db.runDbMigrationsUsingResource(
          postgresqlConfig.flywayResource(fly4sConfig, classloaderForMigrations),
          recreateSchemaAndRetryOnFailure = true,
          log,
        )(using transactor = transactor, tracer = Tracer.noop)
      )
    } yield transactor
}
