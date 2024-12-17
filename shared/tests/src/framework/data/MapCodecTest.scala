package framework.data

import framework.utils.FrameworkTestSuite
import cats.syntax.all.*

class MapCodecTest extends FrameworkTestSuite {
  case class Person(id: Int, name: String)
  val person = Person(123, "John")

  val idCodec = MapCodec.PartCodec.at("id")((_: String).toIntOption.toRight("not an int"))(_.toString)
  val nameCodec = MapCodec.PartCodec.at("name")((s: String) => Right(s))(identity)

  val personCodec = (idCodec.asCodec, nameCodec.asCodec).imapN(Person.apply)(Tuple.fromProductTyped)

  def testEncodeDecode[K, V, A](codec: MapCodec[K, V, A], input: A) = {
    val encoded = codec.encode(input)
    val decoded = codec.decode(encoded)

    decoded shouldBe Right(input)
  }

  test("single key") {
    testEncodeDecode(idCodec, person.id)
    testEncodeDecode(nameCodec, person.name)
  }

  test("composite") {
    testEncodeDecode(personCodec, person)
  }
}
