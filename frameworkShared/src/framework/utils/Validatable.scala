package framework.utils

import io.scalaland.chimney.partial.Result

/** Interface for types that can be validated. */
trait Validatable[-A] {
  def isValid(value: A): Boolean = validate(value).isEmpty

  def validate(value: A): Option[Result.Errors]
}
