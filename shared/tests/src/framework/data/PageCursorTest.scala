package framework.data

import cats.data.NonEmptyVector
import framework.utils.FrameworkTestSuite
import sttp.tapir.Schema
import sttp.tapir.DecodeResult

class PageCursorTest extends FrameworkTestSuite {
  case class Person(id: String, createdAt: Int)

  val currentPage = NonEmptyVector.of(
    Person("0", 1000),
    Person("1", 2000),
  )

  val baseCursor = PageCursor.withPageSize(10)

  test("next from page #0") {
    baseCursor.first.next(currentPage)(_.id, _.createdAt) shouldBe baseCursor.next("1", 2000, 0)
  }

  test("previous from page #2") {
    baseCursor.previous(currentPage, 1)(_.id, _.createdAt) shouldBe baseCursor.previous("0", 1000, 1)
  }
}

class PageCursorBinaryTapirCodecTest extends FrameworkTestSuite {
  val codec = PageCursor.tapirCodecBinaryBase64[String, Int, PageSize]

  enum PageSize derives scodec.Codec, Schema {
    case _10
  }

  test("it serializes back and forth") {
    val cursor = PageCursor.withPageSize(PageSize._10).first.next("1", 200)

    val encoded = codec.encode(cursor)
    codec.decode(encoded) shouldBe DecodeResult.Value(cursor)
  }
}
