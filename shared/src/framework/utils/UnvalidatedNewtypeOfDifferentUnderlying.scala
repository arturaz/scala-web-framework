package framework.utils

import alleycats.Empty
import framework.prelude.*
import framework.exts.*
import framework.utils.Validatable
import io.scalaland.chimney.partial.Result.Errors
import io.scalaland.chimney.{PartialTransformer, Transformer}
import neotype.Newtype
import scala.annotation.targetName

/** Provides a newtype that is unvalidated that is of different underlying type than the validated wrapper.
  *
  * @param unvalidatedExample
  *   any value of type [[TValidatedUnderlying]], helps the type inference. This will not be needed once Scala gains the
  *   ability to specify only some type parameters.
  */
trait UnvalidatedNewtypeOfDifferentUnderlying[
  TUnvalidatedUnderlying,
  TValidatedUnderlying,
  TValidatedWrapperCompanion <: Newtype[TValidatedUnderlying],
](unvalidatedExample: TUnvalidatedUnderlying, val companion: TValidatedWrapperCompanion)(using
  val toValidatedUnderlying: PartialTransformer[TUnvalidatedUnderlying, TValidatedUnderlying],
  val validatedUnderlyingToValidated: Transformer[TValidatedUnderlying, companion.Type],
  val validatedToUnderlying: Transformer[companion.Type, TValidatedUnderlying],
  val fromValidated: Transformer[TValidatedUnderlying, TUnvalidatedUnderlying],
) { self =>
  opaque type Type = TUnvalidatedUnderlying

  given underlyingToValidated: PartialTransformer[TUnvalidatedUnderlying, companion.Type] =
    toValidatedUnderlying.andThen(validatedUnderlyingToValidated)

  inline transparent given instance: UnvalidatedNewtypeOfDifferentUnderlying.WithType[
    Type,
    TUnvalidatedUnderlying,
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

  given Validatable[Type] = Validatable.fromPartialTransformer(partialTransformer)
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
  type WithType[TUnvalidatedWrapper, TUnvalidatedUnderlying, TValidatedWrapper <: Newtype[TUnderlying], TUnderlying] =
    UnvalidatedNewtypeOfDifferentUnderlying[TUnvalidatedUnderlying, TUnderlying, TValidatedWrapper] {
      type Type = TUnvalidatedWrapper
    }

  extension [
    TUnvalidatedWrapper,
    TUnvalidatedUnderlying,
    TValidatedWrapperCompanion <: Newtype[TUnderlying],
    TUnderlying,
  ](
    value: TUnvalidatedWrapper
  )(using
    newType: WithType[TUnvalidatedWrapper, TUnvalidatedUnderlying, TValidatedWrapperCompanion, TUnderlying]
  ) {
    def unwrap: TUnvalidatedUnderlying = newType.unwrap(value)

    /** Validates the value and returns the wrapped value if successful.
      *
      * Returns [[String]] on failure because [[neotype.Newtype.make]] returns an [[Either]] of [[String]].
      */
    def validate: Either[String, newType.TValidatedWrapper] =
      newType.underlyingToValidated.transform(unwrap).asEither.left.map(_.errors.head.message.asString)

    /** Validates the value and returns the wrapped value if successful. */
    def validateAsOption: Option[newType.TValidatedWrapper] =
      newType.underlyingToValidated.transform(unwrap).asOption
  }
}
