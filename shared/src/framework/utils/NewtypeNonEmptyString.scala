package framework.utils

import yantl.Newtype
import yantl.ValidatorRule

/** A newtype wrapping a non-empty [[String]] without surrounding whitespace. */
trait NewtypeNonEmptyString extends NewtypeString {
  type TError = NewtypeNonEmptyString.TError

  override def validatorRules = NewtypeNonEmptyString.validators
}
object NewtypeNonEmptyString {
  type TError = ValidatorRule.HadSurroundingWhitespace | ValidatorRule.WasBlank

  val validators: IArray[ValidatorRule[String, TError]] = IArray(
    ValidatorRule.nonBlankString,
    ValidatorRule.withoutSurroundingWhitespace,
  )
}
