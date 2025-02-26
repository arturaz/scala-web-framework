package framework.config

import framework.utils.NewtypeString

/** A string that is multiline and has `\n` in it. Replaces the `\n` with a new line when creating the value. */
object MultilineString extends NewtypeString with Newtype.WithoutValidation {
  given ConfigDecoder[String, Type] = ConfigDecoder[String].map(apply)
  given Conversion[Type, String] = _.unwrap

  override def apply(value: String): Type = super.apply(value.replace("\\n", "\n"))
}
type MultilineString = MultilineString.Type
