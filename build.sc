// // https://github.com/joan38/mill-scalafix
// import $ivy.`com.goyeau::mill-scalafix::0.3.2`
// import com.goyeau.mill.scalafix.ScalafixModule

import coursier.ivy.IvyRepository
import coursier.maven.MavenRepository
import mill._
import mill.api.Logger
import mill.scalajslib.api.{ModuleKind, ModuleSplitStyle, Report}
import mill.scalalib.scalafmt.ScalafmtModule
import os.{Path, RelPath}

import java.nio.file.Files

import scalalib._
import scalajslib._

/** Run `mill mill.scalalib.Dependency/showUpdates` to check for new versions. */
object Versions {

  /** [[https://scala-lang.org]].
    *
    * See the newest versions in https://github.com/scala/scala3/releases.
    */
  final val Scala = "3.5.2"

  /** Look up the version compatible with [[Scala]] version in https://mvnrepository.com/artifact/com.lihaoyi/ammonite.
    */
  final val Ammonite = "3.0.0-M2-30-486378af"

  /** [[https://scala-js.org]] */
  final val ScalaJS = "1.16.0"

  final val Cats = "2.12.0"
  final val MUnit = "1.0.0"
  final val Tapir = "1.10.12"
  final val Http4s = "0.23.27"
  final val Scribe = "3.15.0"
  final val Doobie = "1.0.0-RC5"
  final val Chimney = "1.2.0"
  final val Circe = "0.14.9"
  final val Monocle = "3.2.0"
  final val Ciris = "3.6.0"
}

/** Barebones Scala build. */
trait BaseScalaModule extends ScalaModule {
  override def scalaVersion = Versions.Scala

  override def ammoniteVersion = Versions.Ammonite
}

/** Barebones Scala.js build. */
trait BaseScalaJSModule extends BaseScalaModule with ScalaJSModule {
  // https://www.scala-js.org/
  override def scalaJSVersion = Versions.ScalaJS

  /** Emit ES-type modules */
  override def moduleKind = ModuleKind.ESModule

  /** Split the JS output into lots of small files so that Vite could reload them quickly.
    *   - small modules for our code
    *   - large modules for external dependencies and standard library
    */
  override def moduleSplitStyle = ModuleSplitStyle.SmallModulesFor(List("app"))
}

/** Scala module for our own non-generated code. */
trait FrameworkScalaModule extends BaseScalaModule with ScalafmtModule /*  with ScalafixModule */ {
  override def scalacOptions = T {
    Seq(
      // Show deprecation warnings
      "-deprecation",
      // Show feature warnings,
      "-feature",
      // Warn if a non-unit value is being discarded implicitly
      "-Wvalue-discard",
      // https://github.com/scala/scala3/pull/18093
      "-Wimplausible-patterns",
      // Treat warnings as errors
      "-Werror",
      // Allow implicit conversions
      "-language:implicitConversions",
      // Allow higher-kinded types
      "-language:higherKinds",
      // Enable strict equality: https://docs.scala-lang.org/scala3/reference/contextual/multiversal-equality.html
      "-language:strictEquality",
      // Increase the number of allowed inlines, we have case classes with loads of fields.
      "-Xmax-inlines:128",
      // Disable significant indentation, it's evil and error-prone.
      "-no-indent",
      // https://www.scala-lang.org/2024/08/19/given-priority-change-3.7.html
      "-source",
      "3.7",
    )
  }

  override def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      IvyRepository.parse(
        "https://arturaz.github.io/packages/ivy2/[organization]/[module](_[scalaVersion])/[revision]/[type]s/[artifact](-[classifier]).[ext]"
      ) match {
        case Left(value)  => throw new Exception(value)
        case Right(value) => value
      },
      MavenRepository("https://arturaz.github.io/packages/maven/repository"),
    )
  }
}

/** ScalaJS module for our own non-generated code. */
trait FrameworkScalaJSModule extends FrameworkScalaModule with BaseScalaJSModule

/** A module shared between JVM and JS. */
trait FrameworkPlatformModule extends FrameworkScalaModule with PlatformScalaModule

val ScalaDefaultImports: Vector[String] = Vector(
  "java.lang",
  "scala",
  "scala.Predef",
)

val FrameworkPreludeImports: Vector[String] = Vector(
  "framework.exts",
  "framework.prelude",
)

def makePreludeImportsCompilerOption(imports: Vector[String]): String =
  s"-Yimports:${imports.mkString(",")}"

trait FrameworkTestModule extends FrameworkScalaModule with TestModule.Munit {
  override def ivyDeps = Agg(
    // Testing framework
    // https://scalameta.org/munit/
    // https://mvnrepository.com/artifact/org.scalameta/munit
    ivy"org.scalameta::munit::${Versions.MUnit}",

    // MUnit support for Cats Effect
    // https://github.com/typelevel/munit-cats-effect
    // https://mvnrepository.com/artifact/org.typelevel/munit-cats-effect
    ivy"org.typelevel::munit-cats-effect::2.0.0",

    // MUnit support for ScalaCheck
    // https://scalameta.org/munit/docs/integrations/scalacheck.html
    // https://mvnrepository.com/artifact/org.scalameta/munit-scalacheck
    ivy"org.scalameta::munit-scalacheck::${Versions.MUnit}",
  )
}

/** Module for tests that use Scala.js. */
trait FrameworkScalaJSTestModule extends FrameworkTestModule with FrameworkScalaJSModule with TestScalaJSModule

/** Code common for all projects that use this stack and shared between JVM and JS. */
object shared extends Module {
  trait SharedModule extends FrameworkPlatformModule {
    override def ivyDeps = Agg(
      // Functional programming library
      // https://typelevel.org/cats/
      // https://mvnrepository.com/artifact/org.typelevel/cats-core
      ivy"org.typelevel::cats-core::${Versions.Cats}",
      // // Library containing data structures which facilitate pure functional programming in the Scala programming
      // // language. Some of these are replacements for structures already present in the Scala standard library, but with improvements in safety, some are data structures for which there is no analogue in the Scala standard library.
      // // https://typelevel.org/cats-collections/
      // // https://mvnrepository.com/artifact/org.typelevel/cats-collections
      // ivy"org.typelevel::cats-collections-core::0.9.8",

      // Utilities for Cats
      // https://typelevel.org/cats/alleycats.html
      // https://mvnrepository.com/artifact/org.typelevel/alleycats-core
      ivy"org.typelevel::alleycats-core::${Versions.Cats}",

      // Typeclass derivation for Cats
      // https://github.com/typelevel/kittens
      // https://mvnrepository.com/artifact/org.typelevel/kittens
      ivy"org.typelevel::kittens::3.3.0",

      // Effect system for Scala
      // https://typelevel.org/cats-effect/
      // https://mvnrepository.com/artifact/org.typelevel/cats-effect
      ivy"org.typelevel::cats-effect::3.5.4",

      // Quickly modify deeply nested case class fields
      // https://www.optics.dev/Monocle/
      // https://mvnrepository.com/artifact/dev.optics/monocle-core
      ivy"dev.optics::monocle-core::${Versions.Monocle}",
      // https://mvnrepository.com/artifact/dev.optics/monocle-macro
      ivy"dev.optics::monocle-macro::${Versions.Monocle}",

      // case class transformations
      // https://chimney.readthedocs.io/en/stable
      // https://mvnrepository.com/artifact/io.scalaland/chimney
      ivy"io.scalaland::chimney::${Versions.Chimney}",
      // https://mvnrepository.com/artifact/io.scalaland/chimney-cats
      ivy"io.scalaland::chimney-cats::${Versions.Chimney}",

      // JSON serialization
      // https://github.com/circe/circe
      // https://mvnrepository.com/artifact/io.circe/circe-core
      ivy"io.circe::circe-core::${Versions.Circe}",
      // https://mvnrepository.com/artifact/io.circe/circe-generic
      ivy"io.circe::circe-generic::${Versions.Circe}",
      // https://mvnrepository.com/artifact/io.circe/circe-parser
      ivy"io.circe::circe-parser::${Versions.Circe}",

      // Macros for source code information
      // https://github.com/com-lihaoyi/sourcecode
      // https://mvnrepository.com/artifact/com.lihaoyi/sourcecode
      ivy"com.lihaoyi::sourcecode::0.4.2",

      // Enumerations for Scala 2
      // https://github.com/lloydmeta/enumeratum
      // https://mvnrepository.com/artifact/com.beachape/enumeratum <: scala.reflect.Enum
      ivy"com.beachape::enumeratum:1.7.4",

      // A friendly newtype library for Scala 3.
      // https://github.com/kitlangton/neotype
      // https://mvnrepository.com/artifact/io.github.kitlangton/neotype
      ivy"io.github.kitlangton::neotype::0.3.0",

      // ULID generation,
      // https://github.com/jkugiya/ulid-scala
      // https://mvnrepository.com/artifact/com.github.jkugiya/ulid-scala
      // https://github.com/jkugiya/ulid-scala/pull/9
      ivy"com.github.jkugiya::ulid-scala::1.0.4-SNAPSHOT",

      // Endpoint description library
      // https://tapir.softwaremill.com/en/latest/
      // https://mvnrepository.com/artifact/com.softwaremill.sttp.tapir/tapir-core
      ivy"com.softwaremill.sttp.tapir::tapir-core::${Versions.Tapir}",
      // https://mvnrepository.com/artifact/com.softwaremill.sttp.tapir/tapir-cats
      ivy"com.softwaremill.sttp.tapir::tapir-cats::${Versions.Tapir}",
      // https://mvnrepository.com/artifact/com.softwaremill.sttp.tapir/tapir-json-circe
      ivy"com.softwaremill.sttp.tapir::tapir-json-circe::${Versions.Tapir}",
      // https://mvnrepository.com/artifact/com.softwaremill.sttp.tapir/tapir-json-pickler
      // ivy"com.softwaremill.sttp.tapir::tapir-json-pickler::${Versions.Tapir}",
      // https://mvnrepository.com/artifact/com.softwaremill.sttp.tapir/tapir-cats-effect
      ivy"com.softwaremill.sttp.tapir::tapir-cats-effect::${Versions.Tapir}",
      // https://mvnrepository.com/artifact/com.softwaremill.sttp.shared/fs2
      ivy"com.softwaremill.sttp.shared::fs2::1.3.19",

      // Reactive router for Laminar
      // https://github.com/raquo/Waypoint
      // https://mvnrepository.com/artifact/com.raquo/waypoint
      //
      // Defined in shared part because sometimes you want to access client-side routes from the server.
      //
      // SNAPSHOT includes https://github.com/raquo/Waypoint/pull/19
      ivy"com.raquo::waypoint::8.1.0-SNAPSHOT",

      // Pretty printing library
      // https://github.com/com-lihaoyi/PPrint
      // https://mvnrepository.com/artifact/com.lihaoyi/pprint
      ivy"com.lihaoyi::pprint::0.9.0",
    )
  }

  object jvm extends SharedModule
  object js extends SharedModule with FrameworkScalaJSModule

  /** Helpers for tests. Applications can use them if they need to. */
  object testInfra extends Module {
    object jvm extends FrameworkPlatformModule with FrameworkTestModule
    object js extends FrameworkPlatformModule with FrameworkScalaJSModule with FrameworkTestModule
  }

  /** Tests for the framework code. */
  object tests extends Module {
    object jvm extends FrameworkPlatformModule with FrameworkTestModule {
      override def moduleDeps: Seq[JavaModule] = Seq(shared.jvm, testInfra.jvm)
    }

    object js extends FrameworkPlatformModule with FrameworkTestModule with FrameworkScalaJSTestModule {
      override def moduleDeps: Seq[JavaModule] = Seq(shared.js, testInfra.js)
    }
  }
}

/** Code common for all projects that use this stack for the client (JS platform). */
object client extends FrameworkScalaJSModule {
  override def moduleDeps = Seq(shared.js)

  override def scalacOptions: Target[Seq[String]] = T {
    super.scalacOptions() ++ Seq(makePreludeImportsCompilerOption(ScalaDefaultImports ++ FrameworkPreludeImports))
  }

  override def ivyDeps = Agg(
    // Reactive UI library for Scala.js
    // https://laminar.dev
    // https://mvnrepository.com/artifact/com.raquo/laminar
    ivy"com.raquo::laminar::17.1.0",

    // An implementation of ExecutionContext in terms of JavaScript's `setImmediate`.
    // https://github.com/scala-js/scala-js-macrotask-executor
    ivy"org.scala-js::scala-js-macrotask-executor::1.1.1",

    // The HTTP client.
    // https://tapir.softwaremill.com/en/latest/client/sttp.html#
    ivy"com.softwaremill.sttp.tapir::tapir-sttp-client::${Versions.Tapir}",

    // https://sttp.softwaremill.com/en/stable/backends/javascript/fetch.html#cats-effect-based
    // We use cats-effect because `Future` based backend does not have cancellation support.
    // https://mvnrepository.com/artifact/com.softwaremill.sttp.client3/cats
    ivy"com.softwaremill.sttp.client3::cats::3.9.7",

    // Java 8 Date/Time API implemented in Scala
    // http://cquiroz.github.io/scala-java-time/
    // https://mvnrepository.com/artifact/io.github.cquiroz/scala-java-time
    ivy"io.github.cquiroz::scala-java-time::2.6.0",

    // Idiomatic Cats Effect and FS2 integrations for Web APIs via scala-js-dom.
    // https://github.com/armanbilge/fs2-dom
    // https://mvnrepository.com/artifact/com.armanbilge/fs2-dom
    ivy"com.armanbilge::fs2-dom::0.1.0-M1",
  )
}

/** Code common for all projects that use this stack for the server (JVM platform). */
object server extends FrameworkScalaModule {
  override def moduleDeps = Seq(shared.jvm)

  override def scalacOptions: Target[Seq[String]] = T {
    super.scalacOptions() ++ Seq(makePreludeImportsCompilerOption(ScalaDefaultImports ++ FrameworkPreludeImports))
  }

  override def ivyDeps = Agg(
    // HTTP endpoint description library
    // https://tapir.softwaremill.com/en/latest/
    // https://mvnrepository.com/artifact/com.softwaremill.sttp.tapir
    ivy"com.softwaremill.sttp.tapir::tapir-http4s-server:${Versions.Tapir}",
    ivy"com.softwaremill.sttp.tapir::tapir-cats-effect:${Versions.Tapir}",
    ivy"com.softwaremill.sttp.tapir::tapir-swagger-ui-bundle:${Versions.Tapir}",
    ivy"com.softwaremill.sttp.tapir::tapir-redoc-bundle:${Versions.Tapir}",

    // HTTP server
    // https://http4s.org/
    // https://mvnrepository.com/artifact/org.http4s/http4s-dsl
    ivy"org.http4s::http4s-dsl:${Versions.Http4s}",
    // https://mvnrepository.com/artifact/org.http4s/http4s-ember-server
    ivy"org.http4s::http4s-ember-server:${Versions.Http4s}",
    // https://mvnrepository.com/artifact/org.http4s/http4s-ember-client
    ivy"org.http4s::http4s-ember-client:${Versions.Http4s}",
    // https://mvnrepository.com/artifact/org.http4s/http4s-circe
    ivy"org.http4s::http4s-circe:${Versions.Http4s}",

    // Pure Scala Logging library
    // https://github.com/outr/scribe
    // https://mvnrepository.com/artifact/com.outr/scribe
    ivy"com.outr::scribe:${Versions.Scribe}",
    ivy"com.outr::scribe-cats:${Versions.Scribe}",
    // immudb and http4s uses SLF4J
    // https://github.com/outr/scribe/wiki/slf4j
    ivy"com.outr::scribe-slf4j2:${Versions.Scribe}",

    // Flyway database migrations wrapped in cats-effect
    // https://github.com/geirolz/fly4s
    // https://mvnrepository.com/artifact/com.github.geirolz/fly4s
    ivy"com.github.geirolz::fly4s:1.0.5",
    // https://documentation.red-gate.com/flyway/flyway-cli-and-api/supported-databases/postgresql-database
    // https://mvnrepository.com/artifact/org.flywaydb/flyway-database-postgresql
    ivy"org.flywaydb:flyway-database-postgresql:10.15.2",

    // PostgreSQL driver
    // https://jdbc.postgresql.org/
    // https://mvnrepository.com/artifact/org.postgresql/postgresql
    ivy"org.postgresql:postgresql:42.7.3",

    // PostgreSQL database access
    // https://tpolecat.github.io/doobie
    // https://mvnrepository.com/artifact/org.tpolecat/doobie-core
    ivy"org.tpolecat::doobie-core:${Versions.Doobie}",
    // https://mvnrepository.com/artifact/org.tpolecat/doobie-postgres
    ivy"org.tpolecat::doobie-postgres:${Versions.Doobie}",
    // https://mvnrepository.com/artifact/org.tpolecat/doobie-hikari
    ivy"org.tpolecat::doobie-hikari:${Versions.Doobie}",
    // https://mvnrepository.com/artifact/org.tpolecat/doobie-postgres-circe
    ivy"org.tpolecat::doobie-postgres-circe:${Versions.Doobie}",

    // Typesafe doobie (for SQL queries)
    // https://arturaz.github.io/doobie-typesafe
    // https://mvnrepository.com/artifact/io.github.arturaz/doobie-typesafe
    ivy"io.github.arturaz::doobie-typesafe:0.3-c5b1b72-20240719T145942Z-SNAPSHOT",

    // Functional Configurations for Scala
    // https://cir.is/
    // https://mvnrepository.com/artifact/is.cir/ciris
    ivy"is.cir::ciris:${Versions.Ciris}",
    ivy"is.cir::ciris-http4s:${Versions.Ciris}",
    ivy"is.cir::ciris-squants:${Versions.Ciris}",
  )

  object testInfra extends FrameworkTestModule {
    override def moduleDeps: Seq[JavaModule] = Seq(server, shared.testInfra.jvm)
  }

  object tests extends FrameworkTestModule {
    override def moduleDeps: Seq[JavaModule] = Seq(testInfra)

    override def ivyDeps = super.ivyDeps() ++ Agg(
      // In-memory database.
      // https://www.h2database.com/
      // https://mvnrepository.com/artifact/com.h2database/h2
      ivy"com.h2database:h2:2.2.224"
    )
  }
}

/** Copies all existing and new files from [[src]] to [[dst]] by exchanging the directories with a `move`. */
def copyBySwap(
  src: Path,
  dst: Path,
  log: Logger,
): Unit = {
  log.info(s"Copying '$src' to '$dst'")

  if (os.exists(dst)) {
    val dstNew = Path(s"$dst.new")
    val dstOld = Path(s"$dst.old")

    // Copy the files to the new directory
    os.remove.all(dstNew)
    os.copy(src, dstNew, replaceExisting = true, createFolders = true)

    // Try to do an semi-atomic swap
    os.remove.all(dstOld)
    os.move(dst, dstOld)
    os.move(dstNew, dst)

    // Remove the old path
    os.remove.all(dstOld)
  } else {
    os.copy(src, dst, replaceExisting = true, createFolders = true)
  }
}

/** Copies all existing and new files from [[src]] to [[dst]], removes files from [[dst]] which do not exist in [[src]]
  * anymore.
  */
def syncDirectories(
  src: Path,
  dst: Path,
  log: Logger,
  removeOldFiles: Boolean = true,
): Unit = {
  val srcFiles = os.walk(src).iterator.map(_.relativeTo(src)).toVector
  if (!Files.exists(dst.toNIO)) os.makeDir.all(dst)
  val dstFiles = os.walk(dst).iterator.map(_.relativeTo(dst)).toVector

  val newFiles = srcFiles diff dstFiles
  val removedFiles = dstFiles diff srcFiles
  val keptFiles = dstFiles intersect srcFiles

  log.info(s"Syncing directories: $src -> $dst")
  log.info(s"  New files: ${newFiles.size}")
  if (log.debugEnabled) log.debug(s"  New files:\n    ${newFiles.mkString(",\n    ")}")
  log.info(s"  Kept files: ${keptFiles.size}")
  if (log.debugEnabled) log.debug(s"  Kept files:\n    ${keptFiles.mkString(",\n    ")}")
  if (removeOldFiles) {
    log.info(s"  Removed files: ${removedFiles.size}")
    if (log.debugEnabled) log.debug(s"  Removed files:\n    ${removedFiles.mkString(",\n    ")}")
  }

  val toCopy = newFiles ++ keptFiles
  toCopy.foreach { file =>
    os.copy(src / file, dst / file, replaceExisting = true, createFolders = true)
  }

  if (removeOldFiles) {
    val toRemove = removedFiles
    // Remove from the longest path to the shortest
    toRemove.reverseIterator.foreach { file =>
      os.remove.all(dst / file)
    }
  }
}
