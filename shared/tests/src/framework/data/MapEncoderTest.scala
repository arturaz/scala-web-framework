package framework.data

import framework.utils.FrameworkTestSuite
import cats.syntax.all.*

class MapEncoderTest extends FrameworkTestSuite {
  case class Person(id: Int, name: String)
  val person = Person(123, "John")

  val idEncoder = MapEncoder.PartEncoder.at("id")((_: Person).id.toString)
  val nameEncoder = MapEncoder.PartEncoder.at("name")((_: Person).name)
  val personEncoder = MapEncoder.ofParts(idEncoder, nameEncoder)

  test("single key") {
    idEncoder.encode(person) shouldBe ("id" -> "123")
    nameEncoder.encode(person) shouldBe ("name" -> "John")
  }

  test("composite") {
    personEncoder.encode(person) shouldBe Map("id" -> "123", "name" -> "John")
  }
}
