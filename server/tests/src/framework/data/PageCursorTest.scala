package framework.data

import framework.utils.{DBFixture, FrameworkTestSuite}
import framework.exts.*
import framework.prelude.*
import doobie.TableDefinition.RowHelpers

class PageCursorTest extends FrameworkTestSuite with DBFixture {
  object Docs extends TableDefinition("docs") {
    val id: Column[Int] = Column("id")
    val timestamp: Column[Int] = Column("ts")

    case class Row(id: Int, timestamp: Int)
    object Row
        extends WithSQLDefinition[Row](Composite((id.sqlDef, timestamp.sqlDef))(Row.apply)(Tuple.fromProductTyped))
        with RowHelpers[Row](Docs)
  }

  val withTable = withDDLs(
    sql"create table $Docs (${Docs.id} int not null, ${Docs.timestamp} int not null)",
    // Insert some test data where the `id` is not monotonic and we have some records with the same `ts`.
    sql"""insert into $Docs (${Docs.id}, ${Docs.timestamp}) values
          (10, 0), (11, 0), (12, 0), (0, 1), (1, 1), (2, 1), (20, 2), (21, 2), (22, 2), (23, 2), (24, 2)""",
  )

  type Cursor = PageCursor[Int, Int, Int]
  val baseCursor: PageCursor.Builder[Int] = PageCursor.withPageSize(2)
  val columns = Docs.Row

  def run(cursor: Cursor, order: SqlOrder) =
    sql"""SELECT $columns FROM $Docs WHERE ${cursor.sqlWhereFragment(Docs.id, Docs.timestamp, order)}
          ORDER BY ${cursor.sqlOrderFragment(Docs.id, Docs.timestamp, order)}
          ${cursor.sqlLimitFragment}"""
      .queryOf(columns)
      .to[Vector]
      .flatMap(
        PageCursor.hasSurroundingPages(Docs as "d", None, _, _.c(_.id), _.id, _.c(_.timestamp), _.timestamp, order)
      )

  withTable.test("ASC: page #0 (without cursor)") { xa =>
    val obtained = run(baseCursor.first, SqlOrder.Asc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(10, 0), Docs.Row(11, 0)), hasPrevious = false, hasNext = true),
    )
  }

  withTable.test("ASC: page #0 (from page #1)") { xa =>
    val obtained = run(baseCursor.previous(12, 0, index = 1), SqlOrder.Asc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(10, 0), Docs.Row(11, 0)), hasPrevious = false, hasNext = true),
    )
  }

  withTable.test("ASC: page #1") { xa =>
    val obtained = run(baseCursor.next(11, 0, index = 0), SqlOrder.Asc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(12, 0), Docs.Row(0, 1)), hasPrevious = true, hasNext = true),
    )
  }

  withTable.test("ASC: page #2") { xa =>
    val obtained = run(baseCursor.next(0, 1, index = 1), SqlOrder.Asc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(1, 1), Docs.Row(2, 1)), hasPrevious = true, hasNext = true),
    )
  }

  withTable.test("ASC: page #3") { xa =>
    val obtained = run(baseCursor.next(2, 1, index = 2), SqlOrder.Asc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(20, 2), Docs.Row(21, 2)), hasPrevious = true, hasNext = true),
    )
  }

  withTable.test("ASC: page #4") { xa =>
    val obtained = run(baseCursor.next(21, 2, index = 3), SqlOrder.Asc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(22, 2), Docs.Row(23, 2)), hasPrevious = true, hasNext = true),
    )
  }

  withTable.test("ASC: page #5 (incomplete)") { xa =>
    val obtained = run(baseCursor.next(23, 2, index = 4), SqlOrder.Asc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(24, 2)), hasPrevious = true, hasNext = false),
    )
  }

  withTable.test("DESC: page #0 (without cursor)") { xa =>
    val obtained = run(baseCursor.first, SqlOrder.Desc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(24, 2), Docs.Row(23, 2)), hasPrevious = false, hasNext = true),
    )
  }

  withTable.test("DESC: page #0 (from page #1)") { xa =>
    val obtained = run(baseCursor.previous(22, 2, index = 1), SqlOrder.Desc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(24, 2), Docs.Row(23, 2)), hasPrevious = false, hasNext = true),
    )
  }

  withTable.test("DESC: page #1") { xa =>
    val obtained = run(baseCursor.next(23, 2, index = 0), SqlOrder.Desc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(22, 2), Docs.Row(21, 2)), hasPrevious = true, hasNext = true),
    )
  }

  withTable.test("DESC: page #2") { xa =>
    val obtained = run(baseCursor.next(21, 2, index = 1), SqlOrder.Desc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(20, 2), Docs.Row(2, 1)), hasPrevious = true, hasNext = true),
    )
  }

  withTable.test("DESC: page #3") { xa =>
    val obtained = run(baseCursor.next(2, 1, index = 2), SqlOrder.Desc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(1, 1), Docs.Row(0, 1)), hasPrevious = true, hasNext = true),
    )
  }

  withTable.test("DESC: page #4") { xa =>
    val obtained = run(baseCursor.next(0, 1, index = 3), SqlOrder.Desc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(12, 0), Docs.Row(11, 0)), hasPrevious = true, hasNext = true),
    )
  }

  withTable.test("DESC: page #5 (incomplete)") { xa =>
    val obtained = run(baseCursor.next(11, 0, index = 4), SqlOrder.Desc).transact(xa)

    assertIO(
      obtained,
      HasSurroundingPages(Vector(Docs.Row(10, 0)), hasPrevious = true, hasNext = false),
    )
  }
}
