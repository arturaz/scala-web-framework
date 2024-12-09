package framework.utils.db

import cats.effect.IO
import cats.syntax.traverse.*
import doobie.*
import doobie.free.connection
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.util.fragment.Fragment

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.sql.Types
import java.util.UUID
import scala.collection.mutable
import scala.util.Using
import org.postgresql.geometric.PGpoint

object DumpData {
  private case class TableInfo(
    tableName: String,
    columns: List[String],
  )

  private case class ForeignKeyInfo(
    tableName: String,
    referencedTableName: String,
    constraintName: String,
    isDeferrable: Boolean,
  )

  private case class TopologicalSortResult(
    sorted: List[String],
    circular: Set[String],
    circularDependencies: List[ForeignKeyInfo],
  )

  /** Get foreign key relationships between tables */
  private def getForeignKeyGraph: ConnectionIO[List[ForeignKeyInfo]] =
    Fragment
      .const("""
      SELECT DISTINCT
        tc.table_name,
        ccu.table_name as referenced_table_name,
        tc.constraint_name,
        tc.is_deferrable = 'YES' as is_deferrable
      FROM 
        information_schema.table_constraints AS tc 
        JOIN information_schema.constraint_column_usage AS ccu
          ON ccu.constraint_name = tc.constraint_name
          AND ccu.table_schema = tc.table_schema
      WHERE tc.constraint_type = 'FOREIGN KEY'
        AND tc.table_schema = 'public'
    """)
      .query[ForeignKeyInfo]
      .to[List]

  /** Sort tables in topological order based on their foreign key dependencies. Returns both sorted tables and tables
    * involved in circular dependencies.
    */
  private def sortTablesTopologically(
    tables: Set[String],
    foreignKeys: List[ForeignKeyInfo],
  ): TopologicalSortResult = {
    val graph = mutable.Map[String, mutable.Set[String]]()
    val inDegree = mutable.Map[String, Int]()

    // Initialize graph
    tables.foreach { table =>
      graph(table) = mutable.Set[String]()
      inDegree(table) = 0
    }

    // Build graph
    foreignKeys.foreach { fk =>
      if (tables.contains(fk.tableName) && tables.contains(fk.referencedTableName)) {
        graph(fk.referencedTableName).add(fk.tableName)
        inDegree(fk.tableName) = inDegree(fk.tableName) + 1
      }
    }

    // Topological sort using Kahn's algorithm
    val queue = mutable.Queue[String]()
    val result = mutable.ArrayBuffer[String]()

    // Add all nodes with no dependencies to queue
    tables.foreach { table =>
      if (inDegree(table) == 0) queue.enqueue(table)
    }

    while (queue.nonEmpty) {
      val table = queue.dequeue()
      result += table

      graph(table).foreach { dependent =>
        inDegree(dependent) = inDegree(dependent) - 1
        if (inDegree(dependent) == 0) queue.enqueue(dependent)
      }
    }

    // Find circular dependencies
    val remaining = tables -- result.toSet
    val circularDeps = foreignKeys.filter { fk =>
      remaining.contains(fk.tableName) && remaining.contains(fk.referencedTableName)
    }

    TopologicalSortResult(
      sorted = result.toList,
      circular = remaining,
      circularDependencies = circularDeps,
    )
  }

  /** Dumps all data from the database into an SQL string, excluding the `flyway_schema_history` table.
    *
    * @param xa
    *   Doobie transactor
    */
  def dataDumps(
    excludedTables: Set[String] = Set(
      "spatial_ref_sys", // PostGIS spatial reference system
      "flyway_schema_history", // Flyway schema history table
    )
  )(using xa: Transactor[IO]): IO[String] = {
    def getTables: ConnectionIO[List[TableInfo]] = {
      val excludedTablesFragment =
        if (excludedTables.nonEmpty)
          Fragment.const(s"table_name NOT IN (${excludedTables.map(t => s"'$t'").mkString(", ")})")
        else Fragment.const("1=1")

      for {
        tables <- sql"""
          SELECT table_name 
          FROM information_schema.tables 
          WHERE table_schema = 'public' 
            AND table_type = 'BASE TABLE' 
            AND $excludedTablesFragment
        """
          .query[String]
          .to[List]
        tableInfos <- tables.traverse { tableName =>
          Fragment
            .const(show"""
            SELECT column_name 
            FROM information_schema.columns 
            WHERE table_schema = 'public' 
              AND table_name = '$tableName' 
            ORDER BY ordinal_position
          """)
            .query[String]
            .to[List]
            .map(columns => TableInfo(tableName, columns))
        }
      } yield tableInfos
    }

    def dumpTable(tableName: String, columns: List[String]): ConnectionIO[String] = {
      connection.raw { conn =>
        val stmt = conn.prepareStatement(show"SELECT ${columns.mkString(", ")} FROM $tableName")
        val rows = scala.collection.mutable.ArrayBuffer[String]()

        Using(stmt.executeQuery()) { rs =>
          val metaData = rs.getMetaData
          val columnCount = metaData.getColumnCount

          while (rs.next()) {
            val values = (1 to columnCount).iterator.map { i =>
              val value = Option(rs.getObject(i))
              val sqlType = metaData.getColumnType(i)
              resultSetValueToSqlString(value, sqlType)
            }
            rows += values.mkString("(", ", ", ")")
          }
        }

        if (rows.isEmpty) ""
        else {
          show"""-- $tableName
                |INSERT INTO $tableName (${columns.mkString(", ")})
                |VALUES
                |  ${rows.mkString(",\n  ")};
                |""".stripMargin
        }
      }
    }

    def resultSetValueToSqlString(value: Option[Any], sqlType: Int): String =
      value match {
        case None => "NULL"
        case Some(v) =>
          sqlType match {
            case Types.VARCHAR | Types.CHAR | Types.LONGVARCHAR | Types.NVARCHAR | Types.NCHAR | Types.LONGNVARCHAR =>
              show"'${v.toString.replace("'", "''")}'"
            case Types.BIT | Types.BOOLEAN =>
              v.toString.toUpperCase
            case Types.TIMESTAMP | Types.TIMESTAMP_WITH_TIMEZONE | Types.DATE | Types.TIME | Types.TIME_WITH_TIMEZONE =>
              s"'$v'"
            case Types.ARRAY =>
              val arr = v.asInstanceOf[java.sql.Array].getArray.asInstanceOf[Array[Object]]
              show"ARRAY[${arr
                  .map(elem =>
                    Option(elem) match {
                      case None    => "NULL"
                      case Some(e) => show"'${e.toString.replace("'", "''")}'"
                    }
                  )
                  .mkString(", ")}]"
            case Types.OTHER if v.toString.startsWith("{") => // Handle PostgreSQL arrays that are returned as strings
              s"'$v'"
            case Types.OTHER if v.isInstanceOf[UUID] => // Handle UUIDs
              show"'${v.toString}'"
            case Types.OTHER if v.isInstanceOf[PGpoint] =>
              val point = v.asInstanceOf[PGpoint]
              show"POINT(${point.x}, ${point.y})"
            case Types.OTHER if Option(v).exists(_.toString.startsWith("0101")) => // Handle PostGIS geography type
              s"'${v.toString}'::geography"
            case _ => v.toString
          }
      }

    val program = for {
      tables <- getTables.map(list => list.iterator.map(t => t.tableName -> t.columns).toMap)
      foreignKeys <- getForeignKeyGraph
      sortResult = sortTablesTopologically(tables.keySet, foreignKeys)
      sortedDumps <- sortResult.sorted.traverse(name => dumpTable(name, tables(name)))
      circularDumps <- sortResult.circular.toList.traverse(name => dumpTable(name, tables(name)))
      deferableConstraints = sortResult.circularDependencies.filter(_.isDeferrable).map(_.constraintName).distinct
      circularDepsComment =
        if (sortResult.circularDependencies.nonEmpty) {
          val nonDeferableConstraints =
            sortResult.circularDependencies.filterNot(_.isDeferrable).map(_.constraintName).distinct

          val deferableComment =
            if (deferableConstraints.nonEmpty)
              s"""-- The following constraints will be deferred:
                 |${deferableConstraints.map(c => s"--   $c").mkString("\n")}
                 |--
                 |""".stripMargin
            else ""

          val nonDeferableComment =
            if (nonDeferableConstraints.nonEmpty)
              s"""-- Warning: The following constraints are not deferrable and may cause issues:
                 |--
                 |${nonDeferableConstraints.map(c => s"--   $c").mkString("\n")}
                 |--
                 |""".stripMargin
            else ""

          val deferConstraintsStmt =
            if (deferableConstraints.nonEmpty)
              show"""-- Defer constraints involved in circular dependencies
                    |SET CONSTRAINTS ${deferableConstraints.mkString(", ")} DEFERRED;
                    |""".stripMargin
            else ""

          val circularDependenciesComment =
            sortResult.circularDependencies
              .map(fk => s"--   ${fk.constraintName}: ${fk.tableName} -> ${fk.referencedTableName}")
              .mkString("\n")

          show"""
              |-- Warning: The following foreign key constraints form circular dependencies:
              |--
              |$circularDependenciesComment
              |--
              |$deferableComment
              |--
              |$nonDeferableComment
              |--
              |
              |$deferConstraintsStmt
              |"""
        } else ""
      restoreConstraintsStmt =
        if (deferableConstraints.nonEmpty) show"\nSET CONSTRAINTS ${deferableConstraints.mkString(", ")} IMMEDIATE;"
        else ""
      content = show"""-- Dumping data in dependency order
                      |
                      |${sortedDumps.mkString("\n\n")}
                      |$circularDepsComment
                      |${if (circularDumps.nonEmpty) circularDumps.mkString("\n\n") else ""}
                      |$restoreConstraintsStmt""".stripMargin
    } yield content

    program.transact(xa)
  }
}
