package framework.utils

import alleycats.Empty
import framework.prelude.*
import framework.utils.Validatable
import io.scalaland.chimney.partial.Result.Errors
import io.scalaland.chimney.{PartialTransformer, Transformer}
import neotype.Newtype
import scala.annotation.targetName

/** Provides a newtype that is unvalidated.
  *
  * Usually when dealing with input forms we have to have a type that is not valid until user inputs things into it.
  *
  * This is an example of how to do it.
  * {{{
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
  TValidatedUnderlying,
  TValidatedWrapperCompanion <: Newtype[TValidatedUnderlying],
](val companionOfValidated: TValidatedWrapperCompanion)(using
  val transformer: PartialTransformer[TValidatedUnderlying, companionOfValidated.Type]
) { self =>
  opaque type Type = TValidatedUnderlying

  inline transparent given instance
    : UnvalidatedNewtypeOf.WithType[Type, TValidatedWrapperCompanion, TValidatedUnderlying] = this

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

  def apply(underlying: TValidatedUnderlying): Type = underlying

  @targetName("applyFromValidatedWrapper")
  def apply(underlying: TValidatedWrapper): Type = companionOfValidated.unwrap(underlying)

  def unwrap(wrapped: Type): TValidatedUnderlying = wrapped

  given CanEqual1[Type] = CanEqual.derived

  /** [[Transformer]] instead of [[Conversion]] that goes from raw type to the newtype because [[Conversion]]s are
    * implicit and we don't want that.
    */
  given Transformer[TValidatedUnderlying, Type] = a => a

  /** Allows transforming from the empty value to the [[Option]] of the newtype.
    *
    * For example: "" -> None, "foo" -> Some(WrappedFoo("foo"))
    */
  given underlyingToUnvalidatedOption(using
    empty: Empty[TValidatedUnderlying]
  ): Transformer[TValidatedUnderlying, Option[Type]] = underlying =>
    if (underlying == empty.empty) None
    else Some(apply(underlying))

  /** Allows transforming from the [[Option]] of the newtype to the empty value.
    *
    * For example: None -> "", Some(WrappedFoo("foo")) -> "foo"
    */
  given optionToUnderlying(using empty: Empty[TValidatedUnderlying]): Transformer[Option[Type], TValidatedUnderlying] =
    _.getOrElse(empty.empty)

  /** Implicit [[Conversion]] from the newtype to the raw type. */
  given Conversion[Type, TValidatedUnderlying] = a => a

  /** [[Transformer]] from the newtype to the raw type. */
  given toUnderlying: Transformer[Type, TValidatedUnderlying] = a => a

  /** [[Transformer]] from the validated newtype to the unvalidated newtype. */
  given fromValidated: Transformer[TValidatedWrapper, Type] = validated => companionOfValidated.unwrap(validated)

  given empty(using empty: Empty[TValidatedUnderlying]): Empty[Type] = Empty(apply(empty.empty))

  given circeCodec(using
    decoder: CirceDecoder[TValidatedUnderlying],
    encoder: CirceEncoder[TValidatedUnderlying],
  ): CirceCodec[Type] =
    CirceCodec.from(decoder, encoder).imap(apply)(unwrap)

  given partialTransformer: PartialTransformer[Type, TValidatedWrapper] = PartialTransformer(transformer.transform(_))

  given Validatable[Type] = Validatable.fromPartialTransformer(partialTransformer)
}
object UnvalidatedNewtypeOf {

  /** @tparam TUnvalidatedWrapper
    *   the type that wraps around [[TUnderlying]] without any validations.
    * @tparam TValidatedWrapper
    *   the type that wraps around [[TUnderlying]] and performs validations.
    * @tparam TUnderlying
    *   the type that is wrapped around by [[TUnvalidatedWrapper]] and [[TValidatedWrapper]].
    */
  type WithType[TUnvalidatedWrapper, TValidatedWrapper <: Newtype[TUnderlying], TUnderlying] =
    UnvalidatedNewtypeOf[TUnderlying, TValidatedWrapper] { type Type = TUnvalidatedWrapper }

  extension [TUnvalidatedWrapper, TValidatedWrapperCompanion <: Newtype[TUnderlying], TUnderlying](
    value: TUnvalidatedWrapper
  )(using
    newType: WithType[TUnvalidatedWrapper, TValidatedWrapperCompanion, TUnderlying],
    validatedCompanion: Newtype.WithType[TUnderlying, newType.TValidatedWrapper],
  ) {
    def unwrap: TUnderlying = newType.unwrap(value)

    /** Validates the value and returns the wrapped value if successful.
      *
      * Returns [[String]] on failure because [[neotype.Newtype.make]] returns an [[Either]] of [[String]].
      */
    def validate: Either[String, newType.TValidatedWrapper] =
      newType.transformer.transform(unwrap).asEither.left.map(_.errors.head.message.asString)

    /** Validates the value and returns the wrapped value if successful. */
    def validateAsOption: Option[newType.TValidatedWrapper] =
      newType.transformer.transform(unwrap).asOption

    /** Returns the companion object of the unvalidated newtype. */
    def companionOf: WithType[TUnvalidatedWrapper, TValidatedWrapperCompanion, TUnderlying] = newType

    /** Returns the companion object of the validated newtype. */
    def companionOfValidated: neotype.Newtype.WithType[TUnderlying, newType.TValidatedWrapper] = validatedCompanion
  }
}
