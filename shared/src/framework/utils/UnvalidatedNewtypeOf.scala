package framework.utils

import alleycats.Empty
import framework.prelude.{*, given}
import io.scalaland.chimney.partial.Result.Errors
import io.scalaland.chimney.{PartialTransformer, Transformer}
import yantl.Newtype
import scala.annotation.targetName
import framework.exts.*
import yantl.Validate
import yantl.Newtype

/** Provides a newtype that is unvalidated.
  *
  * Usually when dealing with input forms we have to have a type that is not valid until user inputs things into it.
  *
  * This is an example of how to do it.
  * {{{
  * // TODO: update docs
  * type OrganizationName = OrganizationName.Type
  * object OrganizationName extends Newtype[String] {
  *   override def validate(input: String): Boolean | String = {
  *     if (input.isBlank()) s"Organization name cannot be blank"
  *     else if (input != input.trim()) s"Organization name cannot contain leading or trailing whitespace"
  *     else true
  *   }
  * }
  *
  * type OrganizationNameForm = OrganizationNameForm.Type
  * object OrganizationNameForm extends UnvalidatedNewtypeOf(OrganizationName) {
  *   // This line is optional, see the documentation for [[UnvalidatedNewtypeOf#TValidatedWrapper]]
  *   override type TValidatedWrapper = OrganizationName
  * }
  * }}}
  *
  * `OrganizationNameForm` will get a `.validate` extension method that will turn it into an `OrganizationName`.
  *
  * @param companionOfValidated
  *   the companion object of the validated newtype
  */
trait UnvalidatedNewtypeOf[
  TUnderlying,
  TValidationError,
  TValidatedWrapperCompanion <: Newtype.WithUnderlyingAndError[TUnderlying, TValidationError],
](val companionOfValidated: TValidatedWrapperCompanion) { self =>
  opaque type Type = TUnderlying

  inline transparent given instance
    : UnvalidatedNewtypeOf.WithType[Type, TValidationError, TValidatedWrapperCompanion, TUnderlying] = this

  /** You can do this to improve generated type signatures:
    *
    * {{{
    * object OrganizationNameForm extends UnvalidatedNewtypeOf(OrganizationName) {
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
  type TValidatedWrapper = companionOfValidated.Type

  def apply(underlying: TUnderlying): Type = underlying

  @targetName("applyFromValidatedWrapper")
  def apply(wrapped: TValidatedWrapper): Type = companionOfValidated.unwrap(wrapped)

  given CanEqual1[Type] = CanEqual.derived

  /** [[Transformer]] instead of [[Conversion]] that goes from raw type to the newtype because [[Conversion]]s are
    * implicit and we don't want that.
    */
  given Transformer[TUnderlying, Type] = a => a

  /** Allows transforming from the empty value to the [[Option]] of the newtype.
    *
    * For example: "" -> None, "foo" -> Some(WrappedFoo("foo"))
    */
  given underlyingToUnvalidatedOption(using
    empty: Empty[TUnderlying]
  ): Transformer[TUnderlying, Option[Type]] = underlying =>
    if (underlying == empty.empty) None
    else Some(apply(underlying))

  /** Allows transforming from the [[Option]] of the newtype to the empty value.
    *
    * For example: None -> "", Some(WrappedFoo("foo")) -> "foo"
    */
  given optionToUnderlying(using empty: Empty[TUnderlying]): Transformer[Option[Type], TUnderlying] =
    _.getOrElse(empty.empty)

  /** Implicit [[Conversion]] from the newtype to the raw type. */
  given Conversion[Type, TUnderlying] = a => a

  /** [[Transformer]] from the newtype to the raw type. */
  given toUnderlying: Transformer[Type, TUnderlying] = a => a

  /** [[Transformer]] from the validated newtype to the unvalidated newtype. */
  given fromValidated: Transformer[TValidatedWrapper, Type] = validated => companionOfValidated.unwrap(validated)

  given empty(using empty: Empty[TUnderlying]): Empty[Type] = Empty(apply(empty.empty))

  given circeCodec(using
    decoder: CirceDecoder[TUnderlying],
    encoder: CirceEncoder[TUnderlying],
  ): CirceCodec[Type] =
    CirceCodec.from(decoder, encoder).imap(apply)(unwrap)

  /** Tries to convert from the unvalidated newtype to the validated newtype. */
  given partialTransformer: PartialTransformer[Type, TValidatedWrapper] =
    PartialTransformer.fromEitherStrings(unvalidated => companionOfValidated.make.asStrings(unvalidated))

  given validate: Validate[Type, TValidationError] = companionOfValidated.validate

  extension (unvalidated: Type) {
    def unwrap: TUnderlying = unvalidated

    /** Validates the value and returns the wrapped value if successful.
      *
      * Returns [[String]] on failure because [[yantl.Newtype.make]] returns an [[Either]] of [[String]].
      */
    def validate: Either[NonEmptyVector[TValidationError], TValidatedWrapper] =
      self.companionOfValidated.make(unvalidated).left.map(NonEmptyVector.fromVectorUnsafe)

    /** Validates the value and returns the wrapped value if successful. */
    def validateAsOption: Option[TValidatedWrapper] =
      self.companionOfValidated.make(unvalidated).toOption

    /** Checks if the value is valid. */
    def isValid: Boolean =
      self.companionOfValidated.make(unvalidated).isRight

    /** Checks if the value is invalid. */
    def isInvalid: Boolean =
      self.companionOfValidated.make(unvalidated).isLeft

    /** Returns the companion object of the unvalidated newtype. */
    def companionOf: UnvalidatedNewtypeOf.WithType[Type, TValidationError, TValidatedWrapperCompanion, TUnderlying] =
      this

    /** Returns the companion object of the validated newtype. */
    def companionOfValidated: TValidatedWrapperCompanion = self.companionOfValidated
  }
}
object UnvalidatedNewtypeOf {

  /** @tparam TUnvalidatedWrapper
    *   the type that wraps around [[TUnderlying]] without any validations.
    * @tparam TValidationError
    *   the type of the validation error.
    * @tparam TValidatedWrapper
    *   the type that wraps around [[TUnderlying]] and performs validations.
    * @tparam TUnderlying
    *   the type that is wrapped around by [[TUnvalidatedWrapper]] and [[TValidatedWrapper]].
    */
  type WithType[
    TUnvalidatedWrapper,
    TValidationError,
    TValidatedWrapper <: Newtype.WithUnderlyingAndError[TUnderlying, TValidationError],
    TUnderlying,
  ] =
    UnvalidatedNewtypeOf[TUnderlying, TValidationError, TValidatedWrapper] { type Type = TUnvalidatedWrapper }
}
