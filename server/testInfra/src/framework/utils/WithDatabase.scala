package framework.utils

import cats.effect.{IO, Resource}
import cats.effect.resource_shared_memoized.ResourceSharedMemoized

import java.sql.Connection

trait WithDatabase {
  export framework.db.*

  /** The transactor to use. */
  def dbResource: Resource[IO, Transactor[IO]]
}
object WithDatabase {

  /** Mix this into your test suite to get an in-memory database for each test. */
  trait InMemory extends WithDatabase {

    /** The JDBC connection for the in-memory database. */
    def connectionResource: Resource[IO, Connection]

    /** The transactor made from [[connectionResource]]. */
    override def dbResource: Resource[IO, Transactor.Aux[IO, Connection]] =
      connectionResource.map(connection => Transactor.fromConnection[IO](connection, Some(doobieLogHandler)))
  }

  /** Mix this into an object when your database is external (like postgresql) and you want all of your tests to share
    * the same connection to that database.
    */
  trait External extends WithDatabase {

    /** The cached version of [[dbResource]]. */
    lazy val dbResourceCached: Resource[IO, Transactor[IO]] =
      ResourceSharedMemoized.memoize(dbResource).unsafeRunSync()(using cats.effect.unsafe.implicits.global)
  }
}
