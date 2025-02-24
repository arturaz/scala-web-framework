package framework.utils

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import framework.exts.*
import munit.*

import java.sql.{Connection, DriverManager}
import cats.effect.SyncIO

/** Gives you access to a newly created in-memory H2 database. */
trait InMemoryDBFixture extends WithDatabase.InMemory { self: CatsEffectSuite & FrameworkTestSuiteHelpers =>
  override def connectionResource: Resource[IO, Connection] = Resource.fromAutoCloseable(IO.blocking {
    // Load the driver
    val _ = Class.forName("org.h2.Driver")
    DriverManager.getConnection("jdbc:h2:mem:", "sa", null)
  })

  /** Provides access to a database. */
  def db: SyncIO[FunFixture[Transactor[IO]]] =
    ResourceFunFixture(dbResource)

  /** Runs the given DDLs in the newly created database. */
  def withDDLs(ddls: Fragment*): SyncIO[FunFixture[Transactor[IO]]] =
    db.map(_.mapAsyncIO { xa =>
      ddls.iterator
        .map(_.update.run)
        .toVector
        .sequence
        .transact(xa)
        .as(xa)
    })
}
