package framework.utils

/** A newtype wrapping a non-empty [[String]] without surrounding whitespace. */
trait NewtypeNonEmptyString extends NewtypeString {

  override def validate(input: String): Boolean | String = {
    if (input.isBlank()) "Cannot be blank."
    else if (input != input.trim()) "Cannot contain leading or trailing whitespace."
    else true
  }
}
