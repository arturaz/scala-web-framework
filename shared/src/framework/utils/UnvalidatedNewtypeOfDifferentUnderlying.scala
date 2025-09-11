package framework.utils

import alleycats.Empty
import framework.exts.*
import framework.prelude.{*, given}
import io.scalaland.chimney.partial.Result.Errors
import io.scalaland.chimney.{PartialTransformer, Transformer}
import yantl.{Newtype, Validate}

import scala.annotation.targetName

extension [Input, Error](self: Validate[Input, Error]) {

  def emapValidateInput[NewInput, NewError >: Error](
    f: NewInput => Either[Vector[NewError], Input]
  ): Validate[NewInput, NewError] =
    input => {
      f(input) match {
        case Left(errors) => errors
        case Right(input) => self.validate(input)
      }
    }
}

/** Provides a newtype that is unvalidated that is of different underlying type than the validated wrapper.
  *
  * For example, you can have a type as `LatLng(latitude: Double, longitude: Double)` and a newtype on that, but the
  * user inputs a "latitude,longitude" string. In that case you can use this trait.
  *
  * @param unvalidatedExample
  *   any value of type [[TValidatedUnderlying]], helps the type inference. This will not be needed once Scala gains the
  *   ability to specify only some type parameters.
  *
  * @tparam TUnvalidatedUnderlying
  *   the underlying type that is wrapped around by this wrapper.
  * @tparam TValidatedValidationError
  *   the validation error type.
  * @tparam TValidatedUnderlying
  *   the underlying type that is wrapped around by the validated wrapper.
  * @tparam TValidatedWrapperCompanion
  *   the companion of the validated wrapper.
  */
trait UnvalidatedNewtypeOfDifferentUnderlying[
  TUnvalidatedUnderlying,
  TUnderlyingMakeError,
  TValidatedValidationError,
  TValidatedUnderlying,
  TValidatedWrapperCompanion <: Newtype.WithUnderlyingAndError[TValidatedUnderlying, TValidatedValidationError],
](unvalidatedExample: TUnvalidatedUnderlying, val companion: TValidatedWrapperCompanion)(using
  val unvalidatedtoValidatedUnderlying: yantl.Make[TUnvalidatedUnderlying, TUnderlyingMakeError, TValidatedUnderlying],
  val validatedUnderlyingToValidated: Transformer[TValidatedUnderlying, companion.Type],
  val validatedToUnderlying: Transformer[companion.Type, TValidatedUnderlying],
  val fromValidated: Transformer[TValidatedUnderlying, TUnvalidatedUnderlying],
) { self =>
  opaque type Type = TUnvalidatedUnderlying

  given underlyingToValidated: PartialTransformer[TUnvalidatedUnderlying, companion.Type] =
    PartialTransformer
      .fromEitherStrings(unvalidated => unvalidatedtoValidatedUnderlying.asStrings(unvalidated))
      .andThen(validatedUnderlyingToValidated)

  inline transparent given instance: UnvalidatedNewtypeOfDifferentUnderlying.WithType[
    Type,
    TUnvalidatedUnderlying,
    TUnderlyingMakeError,
    TValidatedValidationError,
    TValidatedWrapperCompanion,
    TValidatedUnderlying,
  ] = this

  /** You can do this to improve generated type signatures:
    *
    * {{{
    * object OrganizationNameForm extends UnvalidatedNewtypeOfDifferentUnderlying("", OrganizationName) {
    *   override type TValidatedWrapper = OrganizationName
    * }
    * }}}
    *
    * Without this, the generated type signature would be:
    * {{{
    * val a = OrganizationNameForm("test")
    * val b: Either[String, OrganizationNameForm.companion.Type] = a.validate
    * }}}
    *
    * With it, the generated type signature would be:
    * {{{
    * val a = OrganizationNameForm("test")
    * val b: Either[String, OrganizationNameForm.Type] = a.validate
    * }}}
    */
  type TValidatedWrapper = companion.Type

  @targetName("applyFromValidated")
  def apply(validated: TValidatedWrapper): Type = {
    val validatedUnderlying = validatedToUnderlying.transform(validated)
    fromValidated.transform(validatedUnderlying)
  }

  def apply(underlying: TUnvalidatedUnderlying): Type = underlying

  @targetName("applyFromValidatedUnderlying")
  def apply(underlying: TValidatedUnderlying): Type = fromValidated.transform(underlying)

  def unwrap(wrapped: Type): TUnvalidatedUnderlying = wrapped

  given CanEqual1[Type] = CanEqual.derived

  /** [[Transformer]] instead of [[Conversion]] that goes from raw type to the newtype because [[Conversion]]s are
    * implicit and we don't want that.
    */
  given Transformer[TUnvalidatedUnderlying, Type] = apply

  given Transformer[TValidatedWrapper, Type] =
    validatedToUnderlying.andThen(fromValidated)

  /** Allows transforming from the empty value to the [[Option]] of the newtype.
    *
    * For example: "" -> None, "foo" -> Some(WrappedFoo("foo"))
    */
  given underlyingToUnvalidatedOption(using
    empty: Empty[TUnvalidatedUnderlying]
  ): Transformer[TUnvalidatedUnderlying, Option[Type]] = underlying =>
    if (underlying == empty.empty) None
    else Some(apply(underlying))

  /** Allows transforming from the [[Option]] of the newtype to the empty value.
    *
    * For example: None -> "", Some(WrappedFoo("foo")) -> "foo"
    */
  given optionToUnderlying(using
    empty: Empty[TUnvalidatedUnderlying]
  ): Transformer[Option[Type], TUnvalidatedUnderlying] =
    _.getOrElse(empty.empty)

  /** Implicit [[Conversion]] from the newtype to the raw type. */
  given Conversion[Type, TUnvalidatedUnderlying] = a => a

  /** [[Transformer]] from the newtype to the raw type. */
  given toUnderlying: Transformer[Type, TUnvalidatedUnderlying] = a => a

  given partialTransformer: PartialTransformer[Type, TValidatedWrapper] = PartialTransformer(
    underlyingToValidated.transform(_)
  )

  given empty(using empty: Empty[TUnvalidatedUnderlying]): Empty[Type] = Empty(apply(empty.empty))

  given circeCodec(using
    decoder: CirceDecoder[TUnvalidatedUnderlying],
    encoder: CirceEncoder[TUnvalidatedUnderlying],
  ): CirceCodec[Type] =
    CirceCodec.from(decoder, encoder).imap(apply)(unwrap)

  given validate: Validate[Type, TUnderlyingMakeError | TValidatedValidationError] =
    companion.validate.emapValidateInput[Type, TUnderlyingMakeError | TValidatedValidationError] { wrappedUnvalidated =>
      val unvalidatedUnderlying = unwrap(wrappedUnvalidated)
      unvalidatedtoValidatedUnderlying(unvalidatedUnderlying)
    }
}
object UnvalidatedNewtypeOfDifferentUnderlying {

  /** @tparam TUnvalidatedWrapper
    *   the type that wraps around [[TUnderlying]] without any validations.
    * @tparam TUnvalidatedUnderlying
    *   the underlying type that is wrapped around by [[TUnvalidatedWrapper]].
    * @tparam TValidatedWrapper
    *   the type that wraps around [[TUnderlying]] and performs validations.
    * @tparam TUnderlying
    *   the type that is wrapped around by [[TUnvalidatedWrapper]] and [[TValidatedWrapper]].
    */
  type WithType[
    TUnvalidatedWrapper,
    TUnvalidatedUnderlying,
    TUnderlyingMakeError,
    TValidatedValidationError,
    TValidatedWrapper <: Newtype.WithUnderlyingAndError[TUnderlying, TValidatedValidationError],
    TUnderlying,
  ] =
    UnvalidatedNewtypeOfDifferentUnderlying[
      TUnvalidatedUnderlying,
      TUnderlyingMakeError,
      TValidatedValidationError,
      TUnderlying,
      TValidatedWrapper,
    ] {
      type Type = TUnvalidatedWrapper
    }

  extension [
    TUnvalidatedWrapper,
    TUnderlyingMakeError,
    TValidatedValidationError,
    TUnvalidatedUnderlying,
    TValidatedWrapperCompanion <: Newtype.WithUnderlyingAndError[TUnderlying, TValidatedValidationError],
    TUnderlying,
  ](
    value: TUnvalidatedWrapper
  )(using
    newType: WithType[
      TUnvalidatedWrapper,
      TUnvalidatedUnderlying,
      TUnderlyingMakeError,
      TValidatedValidationError,
      TValidatedWrapperCompanion,
      TUnderlying,
    ]
  ) {
    def unwrap: TUnvalidatedUnderlying = newType.unwrap(value)

    /** Validates the value and returns the wrapped value if successful.
      *
      * Returns [[String]] on failure because [[yantl.Newtype.make]] returns an [[Either]] of [[String]].
      */
    def validate: Either[String, newType.TValidatedWrapper] =
      newType.underlyingToValidated.transform(unwrap).asEither.left.map(_.errors.head.message.asString)

    /** Validates the value and returns the wrapped value if successful. */
    def validateAsOption: Option[newType.TValidatedWrapper] =
      newType.underlyingToValidated.transform(unwrap).asOption
  }
}
