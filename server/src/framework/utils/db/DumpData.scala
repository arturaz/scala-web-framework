package framework.utils.db

import cats.effect.IO
import cats.syntax.traverse.*
import doobie.*
import doobie.free.connection
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.util.fragment.Fragment
import org.postgresql.geometric.PGpoint

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.sql.Types
import java.util.UUID
import scala.collection.mutable
import scala.util.Using

object DumpData {
  private case class TableInfo(
    tableName: String,
    columns: List[String],
  )

  private case class ForeignKeyInfo(
    tableName: String,
    referencedTableName: String,
  )

  private case class TopologicalSortResult(
    sorted: List[String],
    circular: Set[String],
    circularDependencies: List[(String, String)],
  )

  /** Get foreign key relationships between tables */
  private def getForeignKeyGraph: ConnectionIO[List[ForeignKeyInfo]] =
    Fragment
      .const("""
      SELECT DISTINCT
        tc.table_name,
        ccu.table_name as referenced_table_name
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
      circularDependencies = circularDeps.map(fk => fk.tableName -> fk.referencedTableName),
    )
  }

  /** Dumps all data from the database into an SQL file, excluding the `flyway_schema_history` table.
    *
    * @param outputPath
    *   Path to the output SQL file
    * @param xa
    *   Doobie transactor
    */
  def dumpToFile(outputPath: Path)(using xa: Transactor[IO]): IO[Unit] = {
    dataDumps.flatMap { content =>
      IO.blocking(Files.writeString(outputPath, content, StandardCharsets.UTF_8)).void
    }
  }

  /** Dumps all data from the database into an SQL string, excluding the `flyway_schema_history` table.
    *
    * @param xa
    *   Doobie transactor
    */
  def dataDumps(using xa: Transactor[IO]): IO[String] = {
    def getTables: ConnectionIO[List[TableInfo]] = for {
      tables <- Fragment
        .const("""
          SELECT table_name 
          FROM information_schema.tables 
          WHERE table_schema = 'public' 
            AND table_type = 'BASE TABLE' 
            AND table_name != 'flyway_schema_history'
        """)
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
            case _ => v.toString
          }
      }

    val program = for {
      tables <- getTables.map(list => list.iterator.map(t => t.tableName -> t.columns).toMap)
      foreignKeys <- getForeignKeyGraph
      sortResult = sortTablesTopologically(tables.keySet, foreignKeys)
      sortedDumps <- sortResult.sorted.traverse(name => dumpTable(name, tables(name)))
      circularDumps <- sortResult.circular.toList.traverse(name => dumpTable(name, tables(name)))
      circularDepsComment =
        if (sortResult.circularDependencies.nonEmpty) {
          show"""
              |-- Warning: The following tables have circular dependencies and their data is dumped last:
              |--
              |${sortResult.circularDependencies.map { case (from, to) => s"-- $from -> $to" }.mkString("\n")}
              |"""
        } else ""
      content = show"""-- Dumping data in dependency order
                      |
                      |${sortedDumps.mkString("\n\n")}
                      |$circularDepsComment
                      |${if (circularDumps.nonEmpty) circularDumps.mkString("\n\n") else ""}""".stripMargin
    } yield content

    program.transact(xa)
  }
}
