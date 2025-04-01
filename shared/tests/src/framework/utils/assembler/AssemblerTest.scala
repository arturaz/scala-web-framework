package framework.utils.assembler

import framework.utils.FrameworkTestSuite

class AssemblerTest extends FrameworkTestSuite {
  test("fromJoinResult: grouping") {
    case class Data(id: Int, name: String, address: String)
    val items = Vector(
      Data(1, "John", "123 Main St"),
      Data(1, "John", "456 Elm St"),
      Data(2, "Jane", "789 Oak St"),
      Data(2, "Jane", "101 Pine St"),
      Data(3, "Bob", "222 Cedar St"),
    )

    case class DataGrouped(id: Int, name: String, addresses: Vector[String])

    val actual = Assembler
      .fromJoinResult(
        items,
        _.id,
        i => Some(i.address),
        (i, addresses) => DataGrouped(i.id, i.name, addresses.toVector),
      )
      .toVector

    val expected = Vector(
      DataGrouped(1, "John", Vector("123 Main St", "456 Elm St")),
      DataGrouped(2, "Jane", Vector("789 Oak St", "101 Pine St")),
      DataGrouped(3, "Bob", Vector("222 Cedar St")),
    )

    actual shouldBe expected
  }

  test("fromJoinResult: grouping 2 levels") {
    case class Data(houseId: Int, houseName: String, address: String, apartmentNo: Int, tenantName: String)

    val items = Vector(
      Data(1, "House 1", "123 Main St", 1, "John"),
      Data(1, "House 1", "123 Main St", 1, "Jane"),
      Data(1, "House 1", "123 Main St", 2, "Bob"),
      Data(2, "House 2", "456 Elm St", 1, "Bob"),
      Data(2, "House 2", "456 Elm St", 2, "Alice"),
      Data(2, "House 2", "456 Elm St", 2, "Charlie"),
      Data(3, "House 3", "789 Oak St", 1, "Charlie"),
    )

    case class Apartment(apartmentNo: Int, tenants: Vector[String])
    case class House(houseId: Int, houseName: String, address: String, apartments: Vector[Apartment])

    val actual = Assembler
      .fromJoinResult(
        items,
        _.houseId,
        i => Some((i.apartmentNo, i.tenantName)),
        (i, apartments) => {
          val groupedApartments = Assembler
            .fromJoinResult(
              apartments.toArray,
              _._1,
              tpl => Some(tpl._2),
              (tpl, tenants) => Apartment(tpl._1, tenants.toVector),
            )
            .toVector

          House(i.houseId, i.houseName, i.address, groupedApartments)
        },
      )
      .toVector

    val expected = Vector(
      House(1, "House 1", "123 Main St", Vector(Apartment(1, Vector("John", "Jane")), Apartment(2, Vector("Bob")))),
      House(2, "House 2", "456 Elm St", Vector(Apartment(1, Vector("Bob")), Apartment(2, Vector("Alice", "Charlie")))),
      House(3, "House 3", "789 Oak St", Vector(Apartment(1, Vector("Charlie")))),
    )

    actual shouldBe expected
  }

  test("fromSeparateQueries: grouping") {
    case class Data(id: Int, name: String, address: String)
    case class ChildData(parentId: Int, name: String)

    case class GroupedData(id: Int, name: String, children: Vector[String])

    val datum = Vector(
      Data(1, "John", "123 Main St"),
      Data(2, "Jane", "789 Oak St"),
      Data(3, "Bob", "222 Cedar St"),
    )

    val childDatum = Vector(
      ChildData(1, "John"),
      ChildData(1, "Jane"),
      ChildData(2, "Bob"),
    )

    val actual = Assembler
      .fromSeparateQueries(
        datum,
        _.id,
        childDatum,
        _.parentId,
        (d, cs) => GroupedData(d.id, d.name, cs.map(_.name).toVector),
      )
      .toVector

    val expected = Vector(
      GroupedData(1, "John", Vector("John", "Jane")),
      GroupedData(2, "Jane", Vector("Bob")),
      GroupedData(3, "Bob", Vector.empty),
    )

    actual shouldBe expected
  }
}
