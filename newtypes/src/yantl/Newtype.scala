package yantl

trait Newtype {
  // Clause interleaving (https://docs.scala-lang.org/sips/clause-interleaving.html) would really help us to have
  // better API here, but we need Scala 3.6 to land before we can use it.
  /** The type of errors that can be encountered while validating a value. */
  type TError

  /** The type that we are going to wrap. */
  type TUnderlying

  /** The wrapped type. */
  opaque type Type = TUnderlying

  /** Allows requiring this companion object in `using` clauses. */
  transparent inline given instance: Newtype.WithType[TUnderlying, Type] = this

  def validators: IArray[Newtype.Validator[TUnderlying, TError]] = IArray.empty

  /** Validates the input using [[validators]]. */
  def validate(input: TUnderlying): Vector[TError] =
    validators.iterator.flatMap(_.validate(input)).toVector

  /** Creates a new instance of the wrapped type, validating it first.
    *
    * @return
    *   [[Left]] if there were errors, [[Right]] otherwise
    */
  def make(input: TUnderlying): Either[Vector[TError], Type] = {
    val errors = validate(input)

    if (errors.isEmpty) Right(input) else Left(errors)
  }

  /** Variant of [[make]] that returns errors as a [[Vector]] of strings. */
  def makeAsStrings(input: TUnderlying): Either[Vector[String], Type] =
    make(input).left.map(_.map(_.toString))

  /** Variant of [[make]] that returns errors as a single English string. */
  def makeAsString(input: TUnderlying): Either[String, Type] =
    make(input).left.map(errorsToString)

  /** Creates a new instance of the wrapped type, validating it first.
    *
    * Should be used only in cases where the input is guaranteed to be valid, like in statically known values.
    *
    * @throws IllegalArgumentException
    *   if there were errors
    */
  def makeOrThrow(input: TUnderlying): Type =
    makeAsString(input) match {
      case Left(value)  => throw new IllegalArgumentException(value)
      case Right(value) => value
    }

  /** Converts a [[Vector]] of errors into an English string. */
  def errorsToString(errors: Vector[TError]): String = {
    if (errors.sizeIs == 1) errors.head.toString
    else s"""Multiple errors (${errors.size}) were encountered while validating a value:
            |
            |- ${errors.mkString("\n\n- ")}""".stripMargin
  }

  extension (v: Type) {

    /** Unwraps the value from the newtype to the underlying type. */
    def unwrap: TUnderlying = v
  }
}
object Newtype {

  /** Helper trait to save some typing.
    *
    * Example:
    * {{{
    * object Email extends Newtype.Of[String]
    * }}}
    */
  trait Of[TUnderlying_] extends Newtype {
    type TUnderlying = TUnderlying_
  }

  /** The `Aux` type helper, to easily specify the underlying type.
    *
    * Example:
    * {{{
    * def doThingsWithStrings(using newType: Newtype.WithUnderlying[String]): newType.Type
    * }}}
    */
  type WithUnderlying[TUnderlying_] = Newtype { type TUnderlying = TUnderlying_ }

  /** The `Aux` type helper, to easily specify the underlying and wrapper types.
    *
    * Example:
    * {{{
    * def doThingsWithStrings[TWrapped](using newType: Newtype.WithType[String, TWrapped]): TWrapped
    * }}}
    */
  type WithType[TUnderlying_, TWrapper] = Newtype {
    type TUnderlying = TUnderlying_
    type Type = TWrapper
  }

  trait Validator[-Input, +Error] {
    def validate(input: Input): Option[Error]
  }
  object Validator {

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
        s"Cannot be longer than $maxLength ${English.plural(maxLength, "character", "characters")}, " +
          s"was $actualLength ${English.plural(actualLength, "character", "characters")}."
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
    def minValue[A](minimum: A)(using num: Numeric[A]): Validator[A, SmallerThan[A]] =
      a => if (num.gt(a, minimum)) None else Some(SmallerThan(minimum, a))

    /** Requires the value to be less than or equal to the maximum. */
    def maxValue[A](maximum: A)(using num: Numeric[A]): Validator[A, LargerThan[A]] =
      a => if (num.lt(a, maximum)) None else Some(LargerThan(maximum, a))

    /** Requires the value to be between the minimum and maximum. */
    def between[A](minimum: A, maximum: A)(using num: Numeric[A]): Validator[A, SmallerThan[A] | LargerThan[A]] =
      a =>
        if (num.lt(a, minimum)) Some(SmallerThan(minimum, a))
        else if (num.gt(a, maximum)) Some(LargerThan(maximum, a))
        else None

    /** Requires the value to not be longer than the maximum length. */
    def maxLength[A](maxLength: Int, getLength: A => Int): Validator[A, MaxLengthExceeded[A]] =
      a => {
        val length = getLength(a)

        if (length > maxLength) Some(MaxLengthExceeded(maxLength, length, a))
        else None
      }

    /** Requires the value to not be longer than the maximum length. */
    def maxLength(maxLength: Int): Validator[String, MaxLengthExceeded[String]] =
      this.maxLength(maxLength, _.length)

    /** Requires the value to be non-empty. */
    def nonEmpty[A](isEmpty: A => Boolean): Validator[A, WasEmpty[A]] =
      a => if (!isEmpty(a)) None else Some(WasEmpty(a))

    /** Requires the string to be non-empty. */
    def nonEmptyString: Validator[String, WasEmpty[String]] =
      nonEmpty(_.isEmpty())

    /** Requires the string to not be blank. */
    def nonBlankString: Validator[String, WasBlank] =
      a => if (a.isBlank()) Some(WasBlank(a)) else None

    /** Requires the string to not have leading or trailing whitespace. */
    def withoutSurroundingWhitespace: Validator[String, HadSurroundingWhitespace] =
      a => if (a != a.trim()) Some(HadSurroundingWhitespace(a)) else None

    object English {
      def plural(count: Int, singular: String, plural: String): String =
        if (count == 1) singular else plural
    }
  }

  /** Combines [[Of]] and [[WithoutValidation]]. */
  trait WithoutValidationOf[TUnderlying_] extends Newtype.Of[TUnderlying_] with WithoutValidation

  /** Mix-in for newtypes that don't need validation logic.
    *
    * Provides an [[apply]] method that always succeeds.
    */
  trait WithoutValidation { self: Newtype =>
    type TError = Nothing

    final override def validators = IArray.empty[Newtype.Validator[Any, TError]]

    final def apply(input: TUnderlying): Type = make(input) match {
      case Left(_)      => throw new Exception("impossible")
      case Right(value) => value
    }
  }
}
