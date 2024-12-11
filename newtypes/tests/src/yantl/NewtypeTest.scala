package yantl

import framework.utils.FrameworkTestSuite

class NewtypeTest extends FrameworkTestSuite {
  object BoundInt
      extends Newtype.ValidatedOf(
        IArray(
          ValidatorRule.minValue[Int](10),
          ValidatorRule.maxValue(100),
        )
      )

  test("smaller than") {
    BoundInt.make(5) shouldBe Left(Vector(ValidatorRule.SmallerThan(10, 5)))
  }

  test("larger than") {
    BoundInt.make(105) shouldBe Left(Vector(ValidatorRule.LargerThan(100, 105)))
  }

  test("in range") {
    BoundInt.make(50) shouldBe Right(BoundInt.make(50).toOption.get)
  }
}
