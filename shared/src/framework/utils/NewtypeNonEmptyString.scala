package framework.utils

import yantl.*

/** A newtype wrapping a non-empty [[String]] without surrounding whitespace. */
trait NewtypeNonEmptyString extends NewtypeString {
  type TError = NewtypeNonEmptyString.TError

  override val validate: Validator[String, TError] = Validator.fromRules(NewtypeNonEmptyString.validators)
}
object NewtypeNonEmptyString {
  type TError = ValidatorRule.HadSurroundingWhitespace | ValidatorRule.WasBlank

  val validators: IArray[ValidatorRule[String, TError]] = IArray(
    ValidatorRule.nonBlankString,
    ValidatorRule.withoutSurroundingWhitespace,
  )
}
