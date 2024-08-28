package framework.data

import framework.utils.FrameworkTestSuite
import framework.exts.*

class IteratorExtsTest extends FrameworkTestSuite {
  test("intersperse empty") {
    Iterator().intersperse(0).toVector shouldBe Vector()
  }

  test("intersperse 1") {
    Iterator(1).intersperse(0).toVector shouldBe Vector(1)
  }

  test("intersperse 2") {
    Iterator(1, 2).intersperse(0).toVector shouldBe Vector(1, 0, 2)
  }

  test("intersperse 3") {
    Iterator(1, 2, 3).intersperse(0).toVector shouldBe Vector(1, 0, 2, 0, 3)
  }

  test("intersperse 4") {
    Iterator(1, 2, 3, 4).intersperse(0).toVector shouldBe Vector(1, 0, 2, 0, 3, 0, 4)
  }
}
