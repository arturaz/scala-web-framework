package framework.utils

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import framework.exts.*
import munit.*

import java.sql.{Connection, DriverManager}

/** Gives you access to a newly created in-memory H2 database. */
trait InMemoryDBFixture extends DBFixture { self: CatsEffectSuite & FrameworkTestSuiteHelpers =>
  override def connectionResource: Resource[IO, Connection] = Resource.fromAutoCloseable(IO.blocking {
    // Load the driver
    val _ = Class.forName("org.h2.Driver")
    DriverManager.getConnection("jdbc:h2:mem:", "sa", null)
  })
}
