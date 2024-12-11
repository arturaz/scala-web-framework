package yantl

/** A single validation rule. */
trait ValidatorRule[-Input, +Error] {
  def validate(input: Input): Option[Error]

  def mapError[NewError](f: Error => NewError): ValidatorRule[Input, NewError] =
    input => validate(input).map(f)

  def mapInput[NewInput](f: NewInput => Input): ValidatorRule[NewInput, Error] =
    input => validate(f(input))

  def mapBoth[NewInput, NewError](
    inputMapper: NewInput => Input,
    errorMapper: Error => NewError,
  ): ValidatorRule[NewInput, NewError] =
    input => validate(inputMapper(input)).map(errorMapper)
}
object ValidatorRule {
  def of[Input, Error](f: Input => Option[Error]): ValidatorRule[Input, Error] =
    input => f(input)

  /** The value must not be smaller than the minimum, but it was. */
  case class SmallerThan[+A](minimum: A, actual: A) derives CanEqual {
    override def toString(): String = s"Cannot be smaller than $minimum, was $actual."
  }

  /** The value must not be greater than the maximum, but it was. */
  case class LargerThan[+A](maximum: A, actual: A) derives CanEqual {
    override def toString(): String = s"Cannot be larger than $maximum, was $actual."
  }

  type OutOfRange[+A] = SmallerThan[A] | LargerThan[A]

  /** The value must not be longer than the maximum length, but it was. */
  case class MaxLengthExceeded[+A](maxLength: Int, actualLength: Int, actual: A) derives CanEqual {
    override def toString(): String =
      s"Cannot be longer than $maxLength ${English.characters(maxLength)}, " +
        s"was $actualLength ${English.characters(actualLength)}."
  }

  /** The value must not be empty, but it was. */
  case class WasEmpty[+A](value: A) derives CanEqual {
    override def toString(): String = "Cannot be empty."
  }

  /** The value was only composed of whitespace. */
  case class WasBlank(value: String) derives CanEqual {
    override def toString(): String = "Cannot be blank."
  }

  /** The value had leading or trailing whitespace. */
  case class HadSurroundingWhitespace(value: String) derives CanEqual {
    override def toString(): String = "Cannot have leading or trailing whitespace."
  }

  /** Requires the value to be greater than or equal to the minimum. */
  def minValue[A](minimum: A)(using num: Numeric[A]): ValidatorRule[A, SmallerThan[A]] =
    a => if (num.gt(a, minimum)) None else Some(SmallerThan(minimum, a))

  /** Requires the value to be less than or equal to the maximum. */
  def maxValue[A](maximum: A)(using num: Numeric[A]): ValidatorRule[A, LargerThan[A]] =
    a => if (num.lt(a, maximum)) None else Some(LargerThan(maximum, a))

  /** Requires the value to be between the minimum and maximum. */
  def between[A](minimum: A, maximum: A)(using num: Numeric[A]): ValidatorRule[A, SmallerThan[A] | LargerThan[A]] =
    a =>
      if (num.lt(a, minimum)) Some(SmallerThan(minimum, a))
      else if (num.gt(a, maximum)) Some(LargerThan(maximum, a))
      else None

  /** Requires the value to not be longer than the maximum length. */
  def maxLength[A](maxLength: Int, getLength: A => Int): ValidatorRule[A, MaxLengthExceeded[A]] =
    a => {
      val length = getLength(a)

      if (length > maxLength) Some(MaxLengthExceeded(maxLength, length, a))
      else None
    }

  /** Requires the value to not be longer than the maximum length. */
  def maxLength(maxLength: Int): ValidatorRule[String, MaxLengthExceeded[String]] =
    this.maxLength(maxLength, _.length)

  /** Requires the value to be non-empty. */
  def nonEmpty[A](isEmpty: A => Boolean): ValidatorRule[A, WasEmpty[A]] =
    a => if (!isEmpty(a)) None else Some(WasEmpty(a))

  /** Requires the string to be non-empty. */
  def nonEmptyString: ValidatorRule[String, WasEmpty[String]] =
    nonEmpty(_.isEmpty())

  /** Requires the string to not be blank. */
  def nonBlankString: ValidatorRule[String, WasBlank] =
    a => if (a.isBlank()) Some(WasBlank(a)) else None

  /** Requires the string to not have leading or trailing whitespace. */
  def withoutSurroundingWhitespace: ValidatorRule[String, HadSurroundingWhitespace] =
    a => if (a != a.trim()) Some(HadSurroundingWhitespace(a)) else None

  object English {
    def plural(count: Long, singular: String, plural: String): String =
      if (count == 1) singular else plural

    def characters(count: Long): String =
      plural(count, "character", "characters")

    def bytes(count: Long): String =
      plural(count, "byte", "bytes")
  }
}
