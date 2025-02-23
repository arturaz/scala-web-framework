package framework.utils

import cats.effect.{IO, Resource, SyncIO}
import cats.syntax.all.*
import doobie.Transactor
import framework.exts.*
import munit.*

import java.sql.{Connection, DriverManager}

/** Gives you access to database. */
trait DBFixture { self: CatsEffectSuite & FrameworkTestSuiteHelpers =>
  export framework.db.*

  /** The JDBC connection. */
  def connectionResource: Resource[IO, Connection]

  /** The transactor. */
  def dbResource: Resource[IO, Transactor.Aux[IO, Connection]] =
    connectionResource.map(connection => Transactor.fromConnection[IO](connection, Some(doobieLogHandler)))

  /** Provides access to a database. */
  def db: SyncIO[FunFixture[Transactor.Aux[IO, Connection]]] =
    ResourceFunFixture(dbResource)

  /** Runs the given DDLs in the newly created database. */
  def withDDLs(ddls: Fragment*): SyncIO[FunFixture[Transactor.Aux[IO, Connection]]] =
    db.map(_.mapAsyncIO { xa =>
      ddls.iterator
        .map(_.update.run)
        .toVector
        .sequence
        .transact(xa)
        .as(xa)
    })
}
