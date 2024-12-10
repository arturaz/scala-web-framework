package framework.utils

import yantl.Newtype

/** A newtype wrapping a non-empty [[String]] without surrounding whitespace. */
trait NewtypeNonEmptyString extends NewtypeString {
  type TError = Newtype.Validator.HadSurroundingWhitespace | Newtype.Validator.WasBlank

  override val validators = IArray(
    Newtype.Validator.nonBlankString,
    Newtype.Validator.withoutSurroundingWhitespace,
  )
}
