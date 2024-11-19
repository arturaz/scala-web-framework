package framework.data

import cats.Show
import cats.kernel.Order
import framework.exts.*
import framework.prelude.*
import io.scalaland.chimney.{PartialTransformer, Transformer}

/** JSON Web Token. */
opaque type JWT = String
object JWT {
  given Show[JWT] = _.token
  given CanEqual[JWT, JWT] = CanEqual.derived

  given Ordering[JWT] = Ordering.by(_.token)
  given Order[JWT] = Order.fromOrdering

  given tapirCodec: TapirCodec[String, JWT, TapirCodecFormat.TextPlain] =
    TapirCodec.string.mapEither(apply)(_.token)

  given Transformer[JWT, String] = _.token
  given PartialTransformer[String, JWT] = PartialTransformer.fromEitherString(apply)

  def apply(input: String): Either[String, JWT] = {
    if (input.count(_ == '.') != 2) Left(s"Invalid JWT, expected to have 3 parts separated by '.': $input")
    else if (!input.forall(c => c.isLetterOrDigit || c == '-' || c == '_' || c == '.'))
      Left(s"Invalid JWT, expected base64url: $input")
    else Right(input)
  }

  extension (jwt: JWT) {
    def token: String = jwt
  }
}
