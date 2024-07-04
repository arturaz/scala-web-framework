// // https://github.com/joan38/mill-scalafix
// import $ivy.`com.goyeau::mill-scalafix::0.3.2`
// import com.goyeau.mill.scalafix.ScalafixModule

import coursier.maven.MavenRepository
import mill.scalalib.scalafmt.ScalafmtModule
import java.nio.file.Files
import mill.api.Logger
import os.{Path, RelPath}
import mill.scalajslib.api.Report
import mill.scalajslib.api.ModuleKind
import mill.scalajslib.api.ModuleSplitStyle
import mill._, scalalib._, scalajslib._

/** Run `mill mill.scalalib.Dependency/showUpdates` to check for new versions. */
object Versions {

  /** [[https://scala-lang.org]]. See which versions are supported by Metals at
    * https://scalameta.org/metals/docs/editors/vscode#requirements.
    */
  final val Scala = "3.4.2"

  /** [[https://scala-js.org]] */
  final val ScalaJS = "1.16.0"

  final val Cats = "2.12.0"
  final val MUnit = "1.0.0"
  final val Tapir = "1.10.9"
  final val Http4s = "0.23.27"
  final val Scribe = "3.13.5"
  final val Doobie = "1.0.0-RC5"
  final val Chimney = "1.0.0"
  final val Circe = "0.14.8"
}

/** Barebones Scala build. */
trait BaseScalaModule extends ScalaModule {
  override def scalaVersion = Versions.Scala
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
      // Treat warnings as errors
      "-Xfatal-warnings",
      // Allow implicit conversions
      "-language:implicitConversions",
      // Allow higher-kinded types
      "-language:higherKinds",
      // Enable strict equality: https://docs.scala-lang.org/scala3/reference/contextual/multiversal-equality.html
      "-language:strictEquality",
      // Increase the number of allowed inlines, we have case classes with loads of fields.
      "-Xmax-inlines:128",
    )
  }

  override def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      MavenRepository("https://arturaz.github.io/packages/ivy2"),
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
      // https://github.com/softwaremill/quicklens
      // https://mvnrepository.com/artifact/com.softwaremill.quicklens/quicklens
      ivy"com.softwaremill.quicklens::quicklens::1.9.7",

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
      ivy"com.beachape::enumeratum:1.7.3",

      // A friendly newtype library for Scala 3.
      // https://github.com/kitlangton/neotype
      // https://mvnrepository.com/artifact/io.github.kitlangton/neotype
      ivy"io.github.kitlangton::neotype::0.2.16",

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
      // SNAPSHOT includes fix for https://github.com/sherpal/url-dsl/pull/22
      ivy"com.raquo::waypoint::8.1.0-SNAPSHOT",
    )
  }

  object jvm extends SharedModule
  object js extends SharedModule with FrameworkScalaJSModule

  object tests extends Module {
    object jvm extends FrameworkPlatformModule with FrameworkTestModule {
      override def moduleDeps: Seq[JavaModule] = Seq(shared.jvm)
    }

    object js extends FrameworkPlatformModule with FrameworkTestModule with FrameworkScalaJSTestModule {
      override def moduleDeps: Seq[JavaModule] = Seq(shared.js)
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
    ivy"com.raquo::laminar::17.0.0",

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
    ivy"io.github.cquiroz::scala-java-time::2.5.0",
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

    // Pure Scala Logging library
    // https://github.com/outr/scribe
    // https://mvnrepository.com/artifact/com.outr/scribe
    ivy"com.outr::scribe:${Versions.Scribe}",
    ivy"com.outr::scribe-cats:${Versions.Scribe}",
    // immudb and http4s uses SLF4J
    // https://github.com/outr/scribe/wiki/slf4j
    ivy"com.outr::scribe-slf4j2:${Versions.Scribe}",

    // ULID generation,
    // https://github.com/jkugiya/ulid-scala
    // https://mvnrepository.com/artifact/com.github.jkugiya/ulid-scala
    ivy"com.github.jkugiya::ulid-scala:1.0.3",

    // Flyway database migrations wrapped in cats-effect
    // https://github.com/geirolz/fly4s
    // https://mvnrepository.com/artifact/com.github.geirolz/fly4s
    ivy"com.github.geirolz::fly4s:1.0.4",
    // https://documentation.red-gate.com/flyway/flyway-cli-and-api/supported-databases/postgresql-database
    // https://mvnrepository.com/artifact/org.flywaydb/flyway-database-postgresql
    ivy"org.flywaydb:flyway-database-postgresql:10.13.0",

    // PostgreSQL driver
    // https://jdbc.postgresql.org/
    // https://mvnrepository.com/artifact/org.postgresql/postgresql
    ivy"org.postgresql:postgresql:42.7.3",

    // PostgreSQL database access
    // https://tpolecat.github.io/doobie
    // https://mvnrepository.com/artifact/org.tpolecat/doobie-core
    // https://mvnrepository.com/artifact/org.tpolecat/doobie-postgres
    // https://mvnrepository.com/artifact/org.tpolecat/doobie-hikari
    ivy"org.tpolecat::doobie-core:${Versions.Doobie}",
    ivy"org.tpolecat::doobie-postgres:${Versions.Doobie}",
    ivy"org.tpolecat::doobie-hikari:${Versions.Doobie}",

    // Typesafe doobie (for SQL queries)
    // https://arturaz.github.io/doobie-typesafe
    // https://mvnrepository.com/artifact/io.github.arturaz/doobie-typesafe
    ivy"io.github.arturaz::doobie-typesafe:0.3-c5b1b72-20240702T133825Z-SNAPSHOT",

    // Encryption library
    // https://developers.google.com/tink/tink-setup#java
    // https://mvnrepository.com/artifact/com.google.crypto.tink/tink
    ivy"com.google.crypto.tink:tink:1.13.0",
    // Tink support for Google Cloud KMS
    // https://mvnrepository.com/artifact/com.google.crypto.tink/tink-gcpkms
    ivy"com.google.crypto.tink:tink-gcpkms:1.10.0",

    // Binary codecs description library
    // https://github.com/scodec/scodec
    // https://mvnrepository.com/artifact/org.scodec/scodec-core
    ivy"org.scodec::scodec-core:2.3.0",
  )

  object tests extends FrameworkTestModule {
    override def moduleDeps: Seq[JavaModule] = Seq(server, shared.tests.jvm)

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
