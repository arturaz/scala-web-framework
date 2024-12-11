package yantl

/** Validates values producing one or more errors. */
trait Validator[-Input, +Error] {

  /** Validates the input.
    *
    * @return
    *   a [[Vector]] of errors. If the vector is empty, the input is valid.
    */
  def validate(input: Input): Vector[Error]

  /** Validates the input, returning `None` if it is valid. */
  def validateAsOption(input: Input): Option[Vector[Error]] = {
    val errors = validate(input)
    if (errors.isEmpty) None else Some(errors)
  }

  /** Validates the input, returning `Left` if it is invalid. */
  def validateAsEither(input: Input): Either[Vector[Error], Unit] = {
    val errors = validate(input)
    if (errors.isEmpty) Right(()) else Left(errors)
  }

  /** Checks if the input is valid. */
  def isValid(input: Input): Boolean =
    validate(input).isEmpty

  /** Variant of [[validate]] that returns errors as a [[Vector]] of strings. */
  def validateAsStrings(input: Input): Vector[String] =
    validate(input).map(_.toString)

  /** Variant of [[validate]] that returns errors as a single English string. */
  def validateAsString(input: Input): Option[String] = {
    val errors = validate(input)
    if (errors.isEmpty) None else Some(Validator.errorsToString(errors))
  }

  def mapInput[NewInput](f: NewInput => Input): Validator[NewInput, Error] =
    input => validate(f(input))

  def mapError[NewError](f: Error => NewError): Validator[Input, NewError] =
    input => validate(input).map(f)

  def mapBoth[NewInput, NewError](
    inputMapper: NewInput => Input,
    errorMapper: Error => NewError,
  ): Validator[NewInput, NewError] =
    input => validate(inputMapper(input)).map(errorMapper)

  /** Combines two validators into one. */
  infix def and[OtherInput <: Input, OtherError](
    that: Validator[OtherInput, OtherError]
  ): Validator[OtherInput, Error | OtherError] =
    input => this.validate(input) ++ that.validate(input)
}
object Validator {
  def of[Input, Error](f: Input => Vector[Error]): Validator[Input, Error] =
    input => f(input)

  def fromRules[Input, Error](rules: Iterable[ValidatorRule[Input, Error]]): Validator[Input, Error] =
    input => rules.iterator.flatMap(_.validate(input)).toVector

  /** Does not validate. */
  val noOp: Validator[Any, Nothing] = _ => Vector.empty

  /** Converts a [[Vector]] of errors into an English string, assuming `Error.toString` returns an English string. */
  def errorsToString[Error](errors: Vector[Error]): String = {
    if (errors.sizeIs == 1) errors.head.toString
    else s"""Multiple errors (${errors.size}) were encountered while validating a value:
        |
        |- ${errors.mkString("\n\n- ")}""".stripMargin
  }
}
