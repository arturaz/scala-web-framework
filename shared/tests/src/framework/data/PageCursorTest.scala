package framework.data

import cats.data.NonEmptyVector
import framework.utils.FrameworkTestSuite

class PageCursorTest extends FrameworkTestSuite {
  case class Person(id: Int, createdAt: Int)

  val currentPage = NonEmptyVector.of(
    Person(0, 1000),
    Person(1, 2000),
  )

  val baseCursor = PageCursor.withPageSize(10)

  test("next from page #0") {
    baseCursor.first.next(currentPage)(_.id, _.createdAt) shouldBe baseCursor.next(1, 2000, 0)
  }

  test("previous from page #2") {
    baseCursor.previous(currentPage, 1)(_.id, _.createdAt) shouldBe baseCursor.previous(0, 1000, 1)
  }
}
