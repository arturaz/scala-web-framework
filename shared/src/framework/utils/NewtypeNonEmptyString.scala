package framework.utils

import yantl.Newtype

/** A newtype wrapping a non-empty [[String]] without surrounding whitespace. */
trait NewtypeNonEmptyString extends NewtypeString {
  type TError = NewtypeNonEmptyString.TError

  override val validators: IArray[Newtype.Validator[String, TError]] = NewtypeNonEmptyString.validators
}
object NewtypeNonEmptyString {
  type TError = Newtype.Validator.HadSurroundingWhitespace | Newtype.Validator.WasBlank

  val validators: IArray[Newtype.Validator[String, TError]] = IArray(
    Newtype.Validator.nonBlankString,
    Newtype.Validator.withoutSurroundingWhitespace,
  )
}
