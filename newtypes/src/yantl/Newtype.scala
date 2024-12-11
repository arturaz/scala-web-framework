package yantl

trait Newtype {

  /** The type of errors that can be encountered while validating a value. */
  type TError

  /** The type that we are going to wrap. */
  type TUnderlying

  /** The wrapped type. */
  opaque type Type = TUnderlying

  /** Allows requiring this companion object in `using` clauses. */
  transparent inline given instance: Newtype.WithType[TUnderlying, Type] = this

  def validatorRules: IArray[ValidatorRule[TUnderlying, TError]] = IArray.empty

  lazy val validator: Validator[TUnderlying, TError] = Validator.fromRules(validatorRules)

  /** Validates the input using [[validators]]. */
  def validate(input: TUnderlying): Vector[TError] = validator.validate(input)

  /** Variant of [[validate]] that returns errors as a [[Vector]] of strings. */
  def validateAsStrings(input: TUnderlying): Vector[String] = validator.validateAsStrings(input)

  /** Variant of [[validate]] that returns errors as a single English string. */
  def validateAsString(input: TUnderlying): Option[String] = validator.validateAsString(input)

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
    make(input).left.map(Validator.errorsToString)

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

  /** Creates a new instance of the wrapped type without validating it. */
  def makeUnsafe(input: TUnderlying): Type =
    input

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

  /** Helper trait to save some typing when object has validators.
    *
    * Example:
    * {{{
    * object Email extends Newtype.ValidatedOf(IArray(
    *   ValidatorRule.nonBlankString, ValidatorRule.withoutSurroundingWhitespace
    * ))
    * }}}
    */
  trait ValidatedOf[TUnderlying_, TError_](override val validatorRules: IArray[ValidatorRule[TUnderlying_, TError_]])
      extends Newtype {
    type TUnderlying = TUnderlying_
    type TError = TError_
  }

  /** The `Aux` type helper, to easily specify the underlying type.
    *
    * Example:
    * {{{
    * def doThingsWithStrings(using newType: Newtype.WithUnderlying[String]): newType.Type
    * }}}
    */
  type WithUnderlying[TUnderlying_] = Newtype { type TUnderlying = TUnderlying_ }

  type WithUnderlyingAndError[TUnderlying_, TError_] = Newtype {
    type TUnderlying = TUnderlying_
    type TError = TError_
  }

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

  /** Combines [[Of]] and [[WithoutValidation]]. */
  trait WithoutValidationOf[TUnderlying_] extends Newtype.Of[TUnderlying_] with WithoutValidation

  /** Mix-in for newtypes that don't need validation logic.
    *
    * Provides an [[apply]] method that always succeeds.
    */
  trait WithoutValidation { self: Newtype =>
    type TError = Nothing

    final override def validatorRules = IArray.empty[ValidatorRule[Any, TError]]

    final def apply(input: TUnderlying): Type = make(input) match {
      case Left(_)      => throw new Exception("impossible")
      case Right(value) => value
    }
  }
}
