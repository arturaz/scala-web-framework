package framework.data

import framework.utils.FrameworkTestSuite
import cats.syntax.all.*
import cats.data.NonEmptyChain

class MapDecoderTest extends FrameworkTestSuite {
  val aDecoder = MapDecoder.PartDecoder.at("a")((_: String).toIntOption.toRight("not an int"))
  val bDecoder = MapDecoder.PartDecoder.at("b")((_: String).toLongOption.toRight("not a long"))
  val cDecoder = MapDecoder.PartDecoder.at("c")((_: String).toLongOption.toRight("not a long"))
  val decoder = (aDecoder.asMapDecoder, bDecoder.asMapDecoder).tupled

  test("single key decode should work") {
    aDecoder.decode(Map("a" -> "1")) shouldBe Right(1)
  }

  test("single key decode should fail on missing key") {
    aDecoder.decode(Map.empty) shouldBe Left(MapDecoder.DecodeFailure.MissingKey("a"))
  }

  test("single key decode should fail on invalid value") {
    aDecoder.decode(Map("a" -> "foo")) shouldBe Left(MapDecoder.DecodeFailure.InvalidValue("a", "foo", "not an int"))
  }

  test("composite decode should work") {
    decoder.decode(Map("a" -> "1", "b" -> "2")) shouldBe Right((1, 2L))
  }

  test("composite decode should fail on missing key") {
    decoder.decode(Map("b" -> "1")) shouldBe Left(NonEmptyChain.one(MapDecoder.DecodeFailure.MissingKey("a")))
    decoder.decode(Map("a" -> "1")) shouldBe Left(NonEmptyChain.one(MapDecoder.DecodeFailure.MissingKey("b")))
  }

  test("composite decode should fail on invalid value") {
    decoder.decode(Map("a" -> "1", "b" -> "foo")) shouldBe Left(
      NonEmptyChain.one(MapDecoder.DecodeFailure.InvalidValue("b", "foo", "not a long"))
    )
    decoder.decode(Map("a" -> "foo", "b" -> "1")) shouldBe Left(
      NonEmptyChain.one(MapDecoder.DecodeFailure.InvalidValue("a", "foo", "not an int"))
    )
    decoder.decode(Map("a" -> "foo", "b" -> "bar")) shouldBe Left(
      NonEmptyChain.of(
        MapDecoder.DecodeFailure.InvalidValue("a", "foo", "not an int"),
        MapDecoder.DecodeFailure.InvalidValue("b", "bar", "not a long"),
      )
    )
  }

  test("switching decoder") {
    val decoder = aDecoder.flatMapDecoder(i => if (i > 0) bDecoder else cDecoder)

    decoder.decode(Map("a" -> "1", "b" -> "2")) shouldBe Right(2L)
    decoder.decode(Map("a" -> "-1", "c" -> "3")) shouldBe Right(3L)
  }
}
