package framework.utils

import cats.effect.IO
import doobie.Transactor
import munit.*

import java.sql.{Connection, DriverManager}
import cats.data.EitherT
import cats.syntax.all.*
import framework.exts.*
import scala.util.chaining.*

trait DBFixture { self: CatsEffectSuite & FrameworkTestSuiteHelpers =>
  export framework.db.*

  /** Provides access to a newly created in-memory H2 database. */
  val db = FunFixture[Transactor.Aux[IO, Connection]](
    setup = _ => {
      // Load the driver
      val _ = Class.forName("org.h2.Driver")
      val connection = DriverManager.getConnection("jdbc:h2:mem:", "sa", null)
      Transactor.fromConnection[IO](connection, Some(doobieLogHandler))
    },
    teardown = _.kernel.close(),
  )

  /** Runs the given DDLs in the newly created database. */
  def withDDLs(ddls: Fragment*): FunFixture[Transactor.Aux[IO, Connection]] =
    db.mapAsyncIO { xa =>
      ddls.iterator
        .map(_.update.run)
        .toVector
        .sequence
        .transact(xa)
        .as(xa)
    }
}
