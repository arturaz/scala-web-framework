package framework

import cats.data.Validated
import cats.syntax.all.*
import doobie.postgres.implicits.*
import doobie.syntax.SqlInterpolator.SingleFragment
import doobie.util.{Read, Write}
import fly4s.Fly4s
import framework.config.PostgresqlConfig
import framework.data.*
import framework.utils.seaweedfs.SeaweedFsFileId
import io.circe.Json
import jkugiya.ulid.ULID
import org.postgresql.geometric.PGpoint
import scribe.mdc.MDC
import scribe.{Level, Scribe}

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

/** Allows you to get all DB related stuff in one place.
  *
  * For JSON it uses JSONB, instead of JSON. Feel free to define your own object (like `app.db` and re-export different
  * things if you want).
  *
  * @note
  *   This is an object not a package because you (yet) can't do `export framework.db.*` in your application code if
  *   this is a package.
  */
object db
// Same thing as in `doobie.implicits` object.
    extends doobie.free.Instances
    // Do not use auto derivation, it is more trouble than it is worth
    // with doobie.generic.AutoDerivation
    with doobie.util.meta.LegacyMeta
    with doobie.syntax.AllSyntax {

  export doobie.postgres.implicits.*
  export doobie.postgres.circe.jsonb.implicits.*
  export doobie.{
    insertInto,
    updateTable,
    AliasedTableDefinition,
    Column,
    Columns,
    Composite,
    ConnectionIO,
    Fragment,
    FragmentExtensions,
    Get,
    Meta,
    Put,
    Query,
    Query0,
    Read,
    SQLDefinition,
    SQLDefinitionRead,
    TableDefinition,
    TableName,
    Transactor,
    TypedFragment,
    TypedMultiFragment,
    Update,
    Update0,
    WithSQLDefinition,
    Write,
  }

  object ConnectionIO {
    // Aliases so you could do `ConnectionIO.pure` and friends.
    export doobie.FC.{pure, raiseError, unit}

    /** Runs the given [[SyncIO]] as a part of the transaction. */
    def from[A](io: SyncIO[A]): ConnectionIO[A] =
      doobie.FC.delay(io.unsafeRunSync())

    /** Runs the given [[ConnectionIO]] only if the condition is true. */
    def whenA[A](condition: Boolean)(io: ConnectionIO[A]): ConnectionIO[Option[A]] =
      if (condition) io.map(Some(_)) else unit.as(None)

    /** Runs the given [[ConnectionIO]] only if the condition is false. */
    def unlessA[A](condition: Boolean)(io: ConnectionIO[A]): ConnectionIO[Option[A]] =
      if (condition) unit.as(None) else io.map(Some(_))

    /** Raises an error if the condition is true. */
    def raiseWhen(condition: Boolean)(error: => Throwable): ConnectionIO[Unit] =
      if (condition) doobie.FC.raiseError(error) else unit

    /** Raises an error if the condition is false. */
    def raiseUnless(condition: Boolean)(error: => Throwable): ConnectionIO[Unit] =
      if (condition) unit else doobie.FC.raiseError(error)
  }

  extension [A](io: SyncIO[A]) {

    /** Runs this [[SyncIO]] as a part of the transaction. */
    def toConnectionIO: ConnectionIO[A] = ConnectionIO.from(io)
  }

  enum SqlOrder derives CanEqual {
    case Asc
    case Desc

    def reverse: SqlOrder = this match {
      case SqlOrder.Asc  => SqlOrder.Desc
      case SqlOrder.Desc => SqlOrder.Asc
    }
  }
  object SqlOrder {
    given Conversion[SqlOrder, Fragment] = {
      case SqlOrder.Asc  => Fragment.const("ASC")
      case SqlOrder.Desc => Fragment.const("DESC")
    }

    given Conversion[SqlOrder, SingleFragment[?]] = SingleFragment.fromFragment(_)
  }

  /** Drops and recreates the current schema. */
  def recreateCurrentSchema: ConnectionIO[Unit] = {
    import doobie.*
    for {
      schemaName <- sql"SELECT current_schema()".query[String].unique
      _ <- Fragment.const(show"DROP SCHEMA IF EXISTS $schemaName CASCADE").update.run
      _ <- Fragment.const(show"CREATE SCHEMA IF NOT EXISTS $schemaName").update.run
      _ <- Fragment.const(show"SET search_path TO $schemaName").update.run
    } yield ()
  }

  given sttpUriPut: Get[sttp.model.Uri] = Get[String].map(sttp.model.Uri.parse(_).getOrThrow)
  given sttpUriGet: Put[sttp.model.Uri] = Put[String].contramap(_.toString)

  given ulidGet: Get[ULID] = Get[UUID].map(uuid => ULID.fromUUID(uuid))
  given ulidPut: Put[ULID] = Put[UUID].contramap(_.uuid)

  given ulidWrapperGet[Wrapper](using newType: yantl.Newtype.WithType[ULID, Wrapper]): Get[Wrapper] =
    Get[ULID].map(newType.make(_).getOrThrow)

  given ulidWrapperPut[Wrapper](using newType: yantl.Newtype.WithType[ULID, Wrapper]): Put[Wrapper] =
    Put[ULID].contramap(newType.unwrap)

  given iArrayGet: Get[IArray[Byte]] = Get[Array[Byte]].map(IArray.unsafeFromArray)
  given iArrayPut: Put[IArray[Byte]] = Put[Array[Byte]].contramap(_.unsafeArray)

  given frameworkDateTimeGet: Get[FrameworkDateTime] = Get[LocalDateTime].map(FrameworkDateTime.apply)
  given frameworkDateTimePut: Put[FrameworkDateTime] = Put[LocalDateTime].contramap(_.ldt)

  given frameworkDateGet: Get[FrameworkDate] = Get[LocalDate].map(FrameworkDate.apply)
  given frameworkDatePut: Put[FrameworkDate] = Put[LocalDate].contramap(_.ld)

  given seaweedFsFileIdGet: Get[SeaweedFsFileId] = doobieGetForNewtype(SeaweedFsFileId)
  given seaweedFsFileIdPut: Put[SeaweedFsFileId] = doobiePutForNewtype(SeaweedFsFileId)

  given latitudeGet: Get[Latitude] = doobieGetForNewtype(Latitude)
  given latitudePut: Put[Latitude] = doobiePutForNewtype(Latitude)

  given longitudeGet: Get[Longitude] = doobieGetForNewtype(Longitude)
  given longitudePut: Put[Longitude] = doobiePutForNewtype(Longitude)

  given versionedDataGet[Version, Data](using
    CirceDecoder[VersionedData[Version, Data]]
  ): Get[VersionedData[Version, Data]] =
    Get[Json].map(_.as[VersionedData[Version, Data]].getOrThrow)
  given versionedDataPut[Version, Data](using
    CirceEncoder[VersionedData[Version, Data]]
  ): Put[VersionedData[Version, Data]] =
    Put[Json].contramap(_.asJson)

  /** @return
    *   `true` if migrations were applied, `false` otherwise
    */
  def runDbMigrationsUsingResource(
    flywayResource: Resource[IO, Fly4s[IO]],
    recreateSchemaAndRetryOnFailure: Boolean,
    log: Scribe[IO],
    logLevel: Level = Level.Info,
  )(using transactor: Transactor[IO], tracer: Tracer[IO], mdc: MDC): IO[Boolean] =
    flywayResource
      .logged("Migrating SQL database", log, logLevel)
      .use(runDbMigrations(_, recreateSchemaAndRetryOnFailure = recreateSchemaAndRetryOnFailure, log, logLevel))

  /** @return
    *   `true` if migrations were applied, `false` otherwise
    */
  def runDbMigrations(
    fly4s: Fly4s[IO],
    recreateSchemaAndRetryOnFailure: Boolean,
    log: Scribe[IO],
    logLevel: Level = Level.Info,
  )(using transactor: Transactor[IO], tracer: Tracer[IO], mdc: MDC): IO[Boolean] = {
    def tryMigration(recreateSchemaAndRetryOnFailure: Boolean): IO[Boolean] = {
      fly4s.validateAndMigrate.flatMap {
        case Validated.Valid(result) =>
          log
            .log(
              logLevel,
              mdc,
              show"${result.migrationsExecuted} SQL migration(s) applied in ${result.getTotalMigrationTime}ms",
            )
            .as(true)

        case Validated.Invalid(errors) =>
          val errorStrings = errors.iterator.map { err =>
            show"""|${err.version} @ ${err.filepath}
                       |  Migration description: ${err.description}
                       |  Error code: ${err.errorDetails.errorCode.toString()}
                       |  Error message: ${err.errorDetails.errorMessage}
                       |""".stripMargin
          }

          log.error(s"SQL migration(s) failed:\n${errorStrings.mkString("\n")}") *>
            (if (recreateSchemaAndRetryOnFailure)
               log.log(
                 logLevel,
                 mdc,
                 "Recreating SQL schema and retrying migrations...",
               ) *> db.recreateCurrentSchema.perform *>
                 log.log(logLevel, mdc, "SQL schema recreated, retrying migration.") *>
                 tryMigration(recreateSchemaAndRetryOnFailure = false)
             else log.log(logLevel, mdc, "SQL migrations failed.").as(false))
      }
    }

    tryMigration(recreateSchemaAndRetryOnFailure)
  }

  /** Export from this to get [[Get]] and [[Put]] instances for [[LatLng]] which uses [[PGpoint]] internally. */
  object WithLatLngBackedByPoint {
    given latLngGet: Get[LatLng] =
      Get[PGpoint].map(p => LatLng(Latitude.make.orThrow(p.y), Longitude.make.orThrow(p.x)))
    given latLngPut: Put[LatLng] = Put[PGpoint].contramap(p => new PGpoint(p.longitude.unwrap, p.latitude.unwrap))
    given latLngRead: Read[LatLng] = Read.fromGet
    given latLngWrite: Write[LatLng] = Write.fromPut
  }
}
