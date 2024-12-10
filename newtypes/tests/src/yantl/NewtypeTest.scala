package yantl

import framework.utils.FrameworkTestSuite

class NewtypeTest extends FrameworkTestSuite {
  object BoundInt extends Newtype.Of[Int] {
    type TError = Newtype.Validator.SmallerThan[Int] | Newtype.Validator.LargerThan[Int]

    override val validators = IArray(
      Newtype.Validator.minValue(10),
      Newtype.Validator.maxValue(100),
    )
  }

  test("smaller than") {
    BoundInt.make(5) shouldBe Left(Vector(Newtype.Validator.SmallerThan(10, 5)))
  }

  test("larger than") {
    BoundInt.make(105) shouldBe Left(Vector(Newtype.Validator.LargerThan(100, 105)))
  }

  test("in range") {
    BoundInt.make(50) shouldBe Right(BoundInt.make(50).toOption.get)
  }
}
