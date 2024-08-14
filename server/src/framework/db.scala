package framework

import doobie.syntax.SqlInterpolator.SingleFragment
import doobie.util.{Read, Write}
import doobie.postgres.implicits.*
import jkugiya.ulid.ULID
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
object db {
  export doobie.implicits.*
  export doobie.postgres.implicits.*
  export doobie.postgres.circe.jsonb.implicits.*
  export doobie.{
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
    TableDefinition,
    TableName,
    Transactor,
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

  given ulidWrapperGet[Wrapper](using newType: neotype.Newtype.WithType[ULID, Wrapper]): Get[Wrapper] =
    Get[ULID].map(newType.make(_).getOrThrow)

  given ulidWrapperPut[Wrapper](using newType: neotype.Newtype.WithType[ULID, Wrapper]): Put[Wrapper] =
    Put[ULID].contramap(newType.unwrap)
}
