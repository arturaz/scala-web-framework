package framework

import doobie.syntax.SqlInterpolator.SingleFragment

/** Allows you to get all DB related stuff in one place.
  *
  * @note
  *   This is an object not a package because you (yet) can't do `export framework.db.*` in your application code if
  *   this is a package.
  */
object db {
  export doobie.implicits.*
  export doobie.postgres.implicits.*
  export doobie.{
    updateTable,
    AliasedTableDefinition,
    Column,
    Columns,
    Composite,
    ConnectionIO,
    Fragment,
    FragmentExtensions,
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
  }

  enum SqlOrder derives CanEqual {
    case Asc
    case Desc
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
}
