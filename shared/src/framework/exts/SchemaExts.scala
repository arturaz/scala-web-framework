package framework.exts

import sttp.tapir.Schema
import scala.collection.immutable.SortedMap
import framework.prelude.CirceKeyEncoder
import cats.kernel.Order
import cats.data.NonEmptyMap

extension [A](schema: Schema[A]) {

  /** Maps the description of the schema if it is not empty. Sets it to `ifEmpty` if it is empty. */
  def mapDescription(ifEmpty: => String)(ifSome: String => String): Schema[A] = {
    val newDescription = schema.description.fold(ifEmpty)(ifSome)
    schema.description(newDescription)
  }
}

extension (obj: Schema.type) {
  inline def schemaForSortedMap[K: Ordering, V: Schema](using encoder: CirceKeyEncoder[K]): Schema[SortedMap[K, V]] =
    Schema.schemaForMap[K, V](encoder.apply).map(map => Some(SortedMap.from(map)))(_.toMap)

  inline def schemaForNonEmptyMap[K, V: Schema](using
    encoder: CirceKeyEncoder[K],
    ord: Order[K],
  ): Schema[NonEmptyMap[K, V]] =
    Schema
      .schemaForMap[K, V](encoder.apply)
      .map(map => NonEmptyMap.fromMap(SortedMap.from(map)(using ord.toOrdering)))(_.toSortedMap.toMap)
}
