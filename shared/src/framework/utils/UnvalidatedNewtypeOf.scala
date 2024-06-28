package framework.utils

import alleycats.Empty
import framework.prelude.*
import framework.utils.Validatable
import io.scalaland.chimney.partial.Result.Errors
import io.scalaland.chimney.{PartialTransformer, Transformer}
import neotype.Newtype

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
  */
trait UnvalidatedNewtypeOf[
  TValidatedUnderlying,
  TValidatedWrapperCompanion <: Newtype[TValidatedUnderlying],
](val companion: TValidatedWrapperCompanion)(using
  val transformer: PartialTransformer[TValidatedUnderlying, companion.Type]
) {
  opaque type Type = TValidatedUnderlying

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
  type TValidatedWrapper = companion.Type

  def apply(underlying: TValidatedUnderlying): Type = underlying

  /** [[Transformer]] instead of [[Conversion]] that goes from raw type to the newtype because [[Conversion]]s are
    * implicit and we don't want that.
    */
  given Transformer[TValidatedUnderlying, Type] = a => a

  /** Implicit [[Conversion]] from the newtype to the raw type. */
  given Conversion[Type, TValidatedUnderlying] = a => a

  /** [[Transformer]] from the newtype to the raw type. */
  given Transformer[Type, TValidatedUnderlying] = a => a

  /** [[Transformer]] from the validated newtype to the unvalidated newtype. */
  given Transformer[TValidatedWrapper, Type] = validated => companion.unwrap(validated)

  given empty(using empty: Empty[TValidatedUnderlying]): Empty[Type] = Empty(apply(empty.empty))

  given circeCodec(using
    decoder: CirceDecoder[TValidatedUnderlying],
    encoder: CirceEncoder[TValidatedUnderlying],
  ): CirceCodec[Type] =
    CirceCodec.from(decoder, encoder).imap(apply)(_.unwrap)

  given partialTransformer: PartialTransformer[Type, TValidatedWrapper] = PartialTransformer(transformer.transform(_))

  given Validatable[Type] with
    override def validate(value: Type): Option[Errors] = partialTransformer.transform(value).asEither.left.toOption

  extension (a: Type) {
    def unwrap: TValidatedUnderlying = a

    /** Validates the value and returns the wrapped value if successful.
      *
      * Returns [[String]] on failure because [[neotype.Newtype.make]] returns an [[Either]] of [[String]].
      */
    def validate: Either[String, TValidatedWrapper] =
      transformer.transform(a).asEither.left.map(_.errors.head.message.asString)
  }
}
