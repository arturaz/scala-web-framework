package framework.exts

import sttp.tapir.Schema

extension [A](schema: Schema[A]) {

  /** Maps the description of the schema if it is not empty. Sets it to `ifEmpty` if it is empty. */
  def mapDescription(ifEmpty: => String)(ifSome: String => String): Schema[A] = {
    val newDescription = schema.description.fold(ifEmpty)(ifSome)
    schema.description(newDescription)
  }
}
