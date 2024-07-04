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

  val ascPages = Vector(
    HasSurroundingPages(Vector(Docs.Row(10, 0), Docs.Row(11, 0)), hasPrevious = false, hasNext = true),
    HasSurroundingPages(Vector(Docs.Row(12, 0), Docs.Row(0, 1)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(Docs.Row(1, 1), Docs.Row(2, 1)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(Docs.Row(20, 2), Docs.Row(21, 2)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(Docs.Row(22, 2), Docs.Row(23, 2)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(Docs.Row(24, 2)), hasPrevious = true, hasNext = false),
  )

  // Forward cursors for ascending order
  val ascFwdCursors = Vector(
    baseCursor.first,
    baseCursor.next(11, 0, index = 0),
    baseCursor.next(0, 1, index = 1),
    baseCursor.next(2, 1, index = 2),
    baseCursor.next(21, 2, index = 3),
    baseCursor.next(23, 2, index = 4),
  )

  // Backward cursors for ascending order
  val ascBwdCursors = Vector(
    baseCursor.previous(12, 0, index = 1),
    baseCursor.previous(1, 1, index = 2),
    baseCursor.previous(20, 2, index = 3),
    baseCursor.previous(22, 2, index = 4),
    baseCursor.previous(24, 2, index = 5),
  )

  val descPages = Vector(
    HasSurroundingPages(Vector(Docs.Row(24, 2), Docs.Row(23, 2)), hasPrevious = false, hasNext = true),
    HasSurroundingPages(Vector(Docs.Row(22, 2), Docs.Row(21, 2)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(Docs.Row(20, 2), Docs.Row(2, 1)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(Docs.Row(1, 1), Docs.Row(0, 1)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(Docs.Row(12, 0), Docs.Row(11, 0)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(Docs.Row(10, 0)), hasPrevious = true, hasNext = false),
  )

  // Forward cursors for descending order
  val descFwdCursors = Vector(
    baseCursor.first,
    baseCursor.next(23, 2, index = 0),
    baseCursor.next(21, 2, index = 1),
    baseCursor.next(2, 1, index = 2),
    baseCursor.next(0, 1, index = 3),
    baseCursor.next(11, 0, index = 4),
  )

  // Backward cursors for descending order
  val descBwdCursors = Vector(
    baseCursor.previous(22, 2, index = 1),
    baseCursor.previous(20, 2, index = 2),
    baseCursor.previous(1, 1, index = 3),
    baseCursor.previous(12, 0, index = 4),
    baseCursor.previous(10, 0, index = 5),
  )

  type Cursor = PageCursor[Int, Int, Int]
  def baseCursor: PageCursor.Builder[Int] = PageCursor.withPageSize(2)
  val columns = Docs.Row

  def run(cursor: Cursor, order: SqlOrder) =
    sql"""SELECT $columns FROM $Docs WHERE ${cursor.sqlWhereFragment(Docs.id, Docs.timestamp, order)}
          ORDER BY ${cursor.sqlOrderFragment(Docs.id, Docs.timestamp, order)}
          ${cursor.sqlLimitFragment}"""
      .queryOf(columns)
      .to[Vector]
      .map(cursor.processResults)
      .flatMap(
        PageCursor.hasSurroundingPages(Docs as "d", None, _, _.c(_.id), _.id, _.c(_.timestamp), _.timestamp, order)
      )

  withTable.test("ASC: page #0 (without cursor)") { xa =>
    val obtained = run(ascFwdCursors(0), SqlOrder.Asc).transact(xa)

    assertIO(obtained, ascPages(0))
  }

  withTable.test("ASC: page #0 (from page #1)") { xa =>
    val obtained = run(ascBwdCursors(0), SqlOrder.Asc).transact(xa)

    assertIO(obtained, ascPages(0))
  }

  withTable.test("ASC: page #1") { xa =>
    val obtained = run(ascFwdCursors(1), SqlOrder.Asc).transact(xa)

    assertIO(obtained, ascPages(1))
  }

  withTable.test("ASC: page #1 (from page #2)") { xa =>
    val obtained = run(ascBwdCursors(1), SqlOrder.Asc).transact(xa)

    assertIO(obtained, ascPages(1))
  }

  withTable.test("ASC: page #2") { xa =>
    val obtained = run(ascFwdCursors(2), SqlOrder.Asc).transact(xa)

    assertIO(obtained, ascPages(2))
  }

  withTable.test("ASC: page #2 (from page #3)") { xa =>
    val obtained = run(ascBwdCursors(2), SqlOrder.Asc).transact(xa)

    assertIO(obtained, ascPages(2))
  }

  withTable.test("ASC: page #3") { xa =>
    val obtained = run(ascFwdCursors(3), SqlOrder.Asc).transact(xa)

    assertIO(obtained, ascPages(3))
  }

  withTable.test("ASC: page #3 (from page #4)") { xa =>
    val obtained = run(ascBwdCursors(3), SqlOrder.Asc).transact(xa)

    assertIO(obtained, ascPages(3))
  }

  withTable.test("ASC: page #4") { xa =>
    val obtained = run(ascFwdCursors(4), SqlOrder.Asc).transact(xa)

    assertIO(obtained, ascPages(4))
  }

  withTable.test("ASC: page #4 (from page #5)") { xa =>
    val obtained = run(ascBwdCursors(4), SqlOrder.Asc).transact(xa)

    assertIO(obtained, ascPages(4))
  }

  withTable.test("ASC: page #5 (incomplete)") { xa =>
    val obtained = run(ascFwdCursors(5), SqlOrder.Asc).transact(xa)

    assertIO(obtained, ascPages(5))
  }

  withTable.test("DESC: page #0 (without cursor)") { xa =>
    val obtained = run(descFwdCursors(0), SqlOrder.Desc).transact(xa)

    assertIO(obtained, descPages(0))
  }

  withTable.test("DESC: page #0 (from page #1)") { xa =>
    val obtained = run(baseCursor.previous(22, 2, index = 1), SqlOrder.Desc).transact(xa)

    assertIO(obtained, descPages(0))
  }

  withTable.test("DESC: page #1") { xa =>
    val obtained = run(descFwdCursors(1), SqlOrder.Desc).transact(xa)

    assertIO(obtained, descPages(1))
  }

  withTable.test("DESC: page #1 (from page #2)") { xa =>
    val obtained = run(descBwdCursors(1), SqlOrder.Desc).transact(xa)

    assertIO(obtained, descPages(1))
  }

  withTable.test("DESC: page #2") { xa =>
    val obtained = run(descFwdCursors(2), SqlOrder.Desc).transact(xa)

    assertIO(obtained, descPages(2))
  }

  withTable.test("DESC: page #2 (from page #3)") { xa =>
    val obtained = run(descBwdCursors(2), SqlOrder.Desc).transact(xa)

    assertIO(obtained, descPages(2))
  }

  withTable.test("DESC: page #3") { xa =>
    val obtained = run(descFwdCursors(3), SqlOrder.Desc).transact(xa)

    assertIO(obtained, descPages(3))
  }

  withTable.test("DESC: page #3 (from page #4)") { xa =>
    val obtained = run(descBwdCursors(3), SqlOrder.Desc).transact(xa)

    assertIO(obtained, descPages(3))
  }

  withTable.test("DESC: page #4") { xa =>
    val obtained = run(descFwdCursors(4), SqlOrder.Desc).transact(xa)

    assertIO(obtained, descPages(4))
  }

  withTable.test("DESC: page #4 (from page #5)") { xa =>
    val obtained = run(descBwdCursors(4), SqlOrder.Desc).transact(xa)

    assertIO(obtained, descPages(4))
  }

  withTable.test("DESC: page #5 (incomplete)") { xa =>
    val obtained = run(descFwdCursors(5), SqlOrder.Desc).transact(xa)

    assertIO(obtained, descPages(5))
  }

  withTable.test("ASC: multiple pages") { xa =>
    def run(c: Cursor) = this.run(c, SqlOrder.Asc).transact(xa)

    for {
      cursor <- IO.pure(baseCursor.first)
      _ <- IO(assertEquals(cursor, ascFwdCursors(0), "forwards cursor #0"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(0), "forwards page #0")

      cursor <- IO.pure(cursor.next(page.data)(_.id, _.timestamp).get)
      _ <- assertIO(IO.pure(cursor), ascFwdCursors(1), "forwards cursor #1")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(1), "forwards page #1")

      cursor <- IO.pure(cursor.next(page.data)(_.id, _.timestamp).get)
      _ <- assertIO(IO.pure(cursor), ascFwdCursors(2), "forwards cursor #2")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(2), "forwards page #2")

      cursor <- IO.pure(cursor.next(page.data)(_.id, _.timestamp).get)
      _ <- assertIO(IO.pure(cursor), ascFwdCursors(3), "forwards cursor #3")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(3), "forwards page #3")

      cursor <- IO.pure(cursor.next(page.data)(_.id, _.timestamp).get)
      _ <- assertIO(IO.pure(cursor), ascFwdCursors(4), "forwards cursor #4")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(4), "forwards page #4")

      cursor <- IO.pure(cursor.next(page.data)(_.id, _.timestamp).get)
      _ <- assertIO(IO.pure(cursor), ascFwdCursors(5), "forwards cursor #5")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(5), "forwards page #5")

      // Run backwards
      cursor <- IO.pure(cursor.previous(page.data)(_.id, _.timestamp).get)
      _ <- assertIO(IO.pure(cursor), ascBwdCursors(4), "backwards cursor #4")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(4), "backwards page #4")

      cursor <- IO.pure(cursor.previous(page.data)(_.id, _.timestamp).get)
      _ <- assertIO(IO.pure(cursor), ascBwdCursors(3), "backwards cursor #3")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(3), "backwards page #3")

      cursor <- IO.pure(cursor.previous(page.data)(_.id, _.timestamp).get)
      _ <- assertIO(IO.pure(cursor), ascBwdCursors(2), "backwards cursor #2")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(2), "backwards page #2")

      cursor <- IO.pure(cursor.previous(page.data)(_.id, _.timestamp).get)
      _ <- assertIO(IO.pure(cursor), ascBwdCursors(1), "backwards cursor #1")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(1), "backwards page #1")

      cursor <- IO.pure(cursor.previous(page.data)(_.id, _.timestamp).get)
      _ <- assertIO(IO.pure(cursor), ascBwdCursors(0), "backwards cursor #0")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(0), "backwards page #0")
    } yield ()
  }

  withTable.test("DESC: multiple pages") { xa =>
    def run(c: Cursor) = this.run(c, SqlOrder.Desc).transact(xa)

    for {
      cursor <- IO.pure(baseCursor.first)
      _ <- IO(assertEquals(cursor, descFwdCursors(0), "forwards cursor #0"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(0), "forwards page #0")

      cursor <- IO.pure(cursor.next(page.data)(_.id, _.timestamp).get)
      _ <- IO(assertEquals(cursor, descFwdCursors(1), "forwards cursor #1"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(1), "forwards page #1")

      cursor <- IO.pure(cursor.next(page.data)(_.id, _.timestamp).get)
      _ <- IO(assertEquals(cursor, descFwdCursors(2), "forwards cursor #2"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(2), "forwards page #2")

      cursor <- IO.pure(cursor.next(page.data)(_.id, _.timestamp).get)
      _ <- IO(assertEquals(cursor, descFwdCursors(3), "forwards cursor #3"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(3), "forwards page #3")

      cursor <- IO.pure(cursor.next(page.data)(_.id, _.timestamp).get)
      _ <- IO(assertEquals(cursor, descFwdCursors(4), "forwards cursor #4"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(4), "forwards page #4")

      cursor <- IO.pure(cursor.next(page.data)(_.id, _.timestamp).get)
      _ <- IO(assertEquals(cursor, descFwdCursors(5), "forwards cursor #5"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(5), "forwards page #5")

      // Run backwards
      cursor <- IO.pure(cursor.previous(page.data)(_.id, _.timestamp).get)
      _ <- IO(assertEquals(cursor, descBwdCursors(4), "backwards cursor #4"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(4), "backwards page #4")

      cursor <- IO.pure(cursor.previous(page.data)(_.id, _.timestamp).get)
      _ <- IO(assertEquals(cursor, descBwdCursors(3), "backwards cursor #3"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(3), "backwards page #3")

      cursor <- IO.pure(cursor.previous(page.data)(_.id, _.timestamp).get)
      _ <- IO(assertEquals(cursor, descBwdCursors(2), "backwards cursor #2"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(2), "backwards page #2")

      cursor <- IO.pure(cursor.previous(page.data)(_.id, _.timestamp).get)
      _ <- IO(assertEquals(cursor, descBwdCursors(1), "backwards cursor #1"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(1), "backwards page #1")

      cursor <- IO.pure(cursor.previous(page.data)(_.id, _.timestamp).get)
      _ <- IO(assertEquals(cursor, descBwdCursors(0), "backwards cursor #0"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(0), "backwards page #0")
    } yield ()
  }
}
