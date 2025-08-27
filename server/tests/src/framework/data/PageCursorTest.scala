package framework.data

import framework.utils.{FrameworkTestSuite, InMemoryDBFixture}
import framework.exts.*
import framework.prelude.{*, given}
import cats.syntax.all.*
import doobie.TableDefinition.RowHelpers
import java.util.Random

trait PageCursorTest(rngSeed: Long) extends FrameworkTestSuite with InMemoryDBFixture {
  object Docs extends TableDefinition("docs") {
    val id: Column[Long] = Column("id")
    val timestamp: Column[Int] = Column("ts")

    case class Row(id: Long, timestamp: Int) {
      def primary = timestamp
      def secondary = id
    }
    object Row
        extends WithSQLDefinition[Row](Composite((id.sqlDef, timestamp.sqlDef))(Row.apply)(Tuple.fromProductTyped))
        with RowHelpers[Row](Docs)
  }

  def printAll(header: String, entries: Vector[Any]): Unit =
    println(s"$header:\n  ${entries.iterator.zipWithIndex.map { case (row, idx) => s"#$idx: $row" }.mkString("\n  ")}")

  val rng = new Random(rngSeed)
  val entries =
    Vector
      .fill(11)(Docs.Row(id = rng.nextInt(0, 1000), timestamp = rng.nextInt(10000, 20000)))
      .sortBy(r => (r.timestamp, r.id))
  printAll("Entries", entries)

  val withTable = withDDLs(
    sql"create table $Docs (${Docs.id} int not null, ${Docs.timestamp} int not null)",
    // Insert some test data where the `id` is not monotonic and we have some records with the same `ts`.
    sql"""insert into $Docs (${Docs.id}, ${Docs.timestamp}) values ${entries
        .map { case Docs.Row(id, ts) => Fragment.const0(s"($id, $ts)") }
        .intercalate(sql", ")}""",
  )

  val ascPages = Vector(
    HasSurroundingPages(Vector(entries(0), entries(1)), hasPrevious = false, hasNext = true),
    HasSurroundingPages(Vector(entries(2), entries(3)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(entries(4), entries(5)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(entries(6), entries(7)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(entries(8), entries(9)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(entries(10)), hasPrevious = true, hasNext = false),
  )
  printAll("ASC pages", ascPages)

  def fwdCursors(pages: Vector[HasSurroundingPages[Vector[Docs.Row]]]): Vector[Cursor] =
    Vector(
      baseCursor.first
    ) ++ pages
      .dropRight(1)
      .iterator
      .map(_.data.last)
      .zipWithIndex
      .map { case (lastRow, idx) =>
        baseCursor.next(primary = lastRow.primary, secondary = lastRow.secondary, index = idx)
      }
      .toVector

  def bwdCursors(pages: Vector[HasSurroundingPages[Vector[Docs.Row]]]): Vector[Cursor] =
    pages.iterator.zipWithIndex
      .drop(1)
      .map { case (page, index) =>
        val firstRow = page.data.head
        baseCursor.previous(primary = firstRow.primary, secondary = firstRow.secondary, index = index)
      }
      .toVector

  // Forward cursors for ascending order
  val ascFwdCursors = fwdCursors(ascPages)
  printAll("ASC fwd cursors", ascFwdCursors)

  // Backward cursors for ascending order
  val ascBwdCursors = bwdCursors(ascPages)
  printAll("ASC bwd cursors", ascBwdCursors)

  val descPages = Vector(
    HasSurroundingPages(Vector(entries(10), entries(9)), hasPrevious = false, hasNext = true),
    HasSurroundingPages(Vector(entries(8), entries(7)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(entries(6), entries(5)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(entries(4), entries(3)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(entries(2), entries(1)), hasPrevious = true, hasNext = true),
    HasSurroundingPages(Vector(entries(0)), hasPrevious = true, hasNext = false),
  )
  printAll("DESC pages", descPages)

  // Forward cursors for descending order
  val descFwdCursors = fwdCursors(descPages)
  printAll("DESC fwd cursors", descFwdCursors)

  // Backward cursors for descending order
  val descBwdCursors = bwdCursors(descPages)
  printAll("DESC bwd cursors", descBwdCursors)

  type Cursor = PageCursor[Int, Long, Int]
  def baseCursor: PageCursor.Builder[Int] = PageCursor.withPageSize(2)
  val columns = Docs.Row

  def run(cursor: Cursor, order: SqlOrder) = {
    val t = Docs

    sql"""SELECT $columns FROM $t WHERE ${cursor.sqlWhereFragment(t.timestamp, t.id, order)}
          ORDER BY ${cursor.sqlOrderFragment(t.timestamp, t.id, order)}
          ${cursor.sqlLimitFragment}"""
      .queryOf(columns, sourcecode.Enclosing.here)
      .to[Vector]
      .map(cursor.processResults)
      .flatMap(
        PageCursor.hasSurroundingPages(t as "d", None, _, t.timestamp, _.primary, t.id, _.secondary, order)
      )
  }

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
    val obtained = run(descBwdCursors(0), SqlOrder.Desc).transact(xa)

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

      cursor <- IO.pure(cursor.next(page.data)(_.primary, _.secondary).get)
      _ <- assertIO(IO.pure(cursor), ascFwdCursors(1), "forwards cursor #1")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(1), "forwards page #1")

      cursor <- IO.pure(cursor.next(page.data)(_.primary, _.secondary).get)
      _ <- assertIO(IO.pure(cursor), ascFwdCursors(2), "forwards cursor #2")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(2), "forwards page #2")

      cursor <- IO.pure(cursor.next(page.data)(_.primary, _.secondary).get)
      _ <- assertIO(IO.pure(cursor), ascFwdCursors(3), "forwards cursor #3")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(3), "forwards page #3")

      cursor <- IO.pure(cursor.next(page.data)(_.primary, _.secondary).get)
      _ <- assertIO(IO.pure(cursor), ascFwdCursors(4), "forwards cursor #4")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(4), "forwards page #4")

      cursor <- IO.pure(cursor.next(page.data)(_.primary, _.secondary).get)
      _ <- assertIO(IO.pure(cursor), ascFwdCursors(5), "forwards cursor #5")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(5), "forwards page #5")

      // Run backwards
      cursor <- IO.pure(cursor.previous(page.data)(_.primary, _.secondary).get)
      _ <- assertIO(IO.pure(cursor), ascBwdCursors(4), "backwards cursor #4")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(4), "backwards page #4")

      cursor <- IO.pure(cursor.previous(page.data)(_.primary, _.secondary).get)
      _ <- assertIO(IO.pure(cursor), ascBwdCursors(3), "backwards cursor #3")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(3), "backwards page #3")

      cursor <- IO.pure(cursor.previous(page.data)(_.primary, _.secondary).get)
      _ <- assertIO(IO.pure(cursor), ascBwdCursors(2), "backwards cursor #2")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(2), "backwards page #2")

      cursor <- IO.pure(cursor.previous(page.data)(_.primary, _.secondary).get)
      _ <- assertIO(IO.pure(cursor), ascBwdCursors(1), "backwards cursor #1")
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), ascPages(1), "backwards page #1")

      cursor <- IO.pure(cursor.previous(page.data)(_.primary, _.secondary).get)
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

      cursor <- IO.pure(cursor.next(page.data)(_.primary, _.secondary).get)
      _ <- IO(assertEquals(cursor, descFwdCursors(1), "forwards cursor #1"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(1), "forwards page #1")

      cursor <- IO.pure(cursor.next(page.data)(_.primary, _.secondary).get)
      _ <- IO(assertEquals(cursor, descFwdCursors(2), "forwards cursor #2"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(2), "forwards page #2")

      cursor <- IO.pure(cursor.next(page.data)(_.primary, _.secondary).get)
      _ <- IO(assertEquals(cursor, descFwdCursors(3), "forwards cursor #3"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(3), "forwards page #3")

      cursor <- IO.pure(cursor.next(page.data)(_.primary, _.secondary).get)
      _ <- IO(assertEquals(cursor, descFwdCursors(4), "forwards cursor #4"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(4), "forwards page #4")

      cursor <- IO.pure(cursor.next(page.data)(_.primary, _.secondary).get)
      _ <- IO(assertEquals(cursor, descFwdCursors(5), "forwards cursor #5"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(5), "forwards page #5")

      // Run backwards
      cursor <- IO.pure(cursor.previous(page.data)(_.primary, _.secondary).get)
      _ <- IO(assertEquals(cursor, descBwdCursors(4), "backwards cursor #4"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(4), "backwards page #4")

      cursor <- IO.pure(cursor.previous(page.data)(_.primary, _.secondary).get)
      _ <- IO(assertEquals(cursor, descBwdCursors(3), "backwards cursor #3"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(3), "backwards page #3")

      cursor <- IO.pure(cursor.previous(page.data)(_.primary, _.secondary).get)
      _ <- IO(assertEquals(cursor, descBwdCursors(2), "backwards cursor #2"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(2), "backwards page #2")

      cursor <- IO.pure(cursor.previous(page.data)(_.primary, _.secondary).get)
      _ <- IO(assertEquals(cursor, descBwdCursors(1), "backwards cursor #1"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(1), "backwards page #1")

      cursor <- IO.pure(cursor.previous(page.data)(_.primary, _.secondary).get)
      _ <- IO(assertEquals(cursor, descBwdCursors(0), "backwards cursor #0"))
      page <- run(cursor)
      _ <- assertIO(IO.pure(page), descPages(0), "backwards page #0")
    } yield ()
  }
}

class PageCursorTest0 extends PageCursorTest(1000)
class PageCursorTest1 extends PageCursorTest(1001)
class PageCursorTest2 extends PageCursorTest(1002)
