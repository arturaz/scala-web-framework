package framework.utils

import cats.syntax.all.*

/** A newtype wrapping a non-empty [[String]] without surrounding whitespace. */
trait NewtypeNonEmptyString extends NewtypeString {
  override def validate(input: String): Boolean | String = {
    val maybeError =
      Option
        .when(input.isBlank())("Cannot be blank.")
        .orElse(Option.when(input != input.trim())("Cannot contain leading or trailing whitespace."))

    maybeError match {
      case None        => super.validate(input)
      case Some(value) => value
    }
  }
}
