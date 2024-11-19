package framework.utils

import cats.Show
import neotype.Newtype
import io.scalaland.chimney.Transformer
import framework.exts.*
import framework.prelude.*
import cats.kernel.Order

/** A newtype wrapping a [[String]]. */
trait NewtypeString extends Newtype[String] {
  given Show[Type] = unwrap(_)
  given CanEqual[Type, Type] = CanEqual.derived

  given Ordering[Type] = Ordering.by(unwrap)
  given Order[Type] = Order.fromOrdering

  given tapirCodec: TapirCodec[String, Type, TapirCodecFormat.TextPlain] =
    TapirCodec.string.map(make(_).getOrThrow)(unwrap)

  given Transformer[Type, String] = unwrap
  given Transformer[String, Type] = make(_).getOrThrow

  /* If specified validates that the length of the string is less than or equal to the specified value. */
  def maxLength: Option[Int] = None

  inline def validateString(input: String): Boolean | String = {
    val maybeError =
      maxLength.flatMap { maxLength =>
        Option.when(input.length() > maxLength)(
          show"Cannot be longer than $maxLength characters, was ${input.length()} characters."
        )
      }

    maybeError match {
      case None        => true
      case Some(value) => value
    }
  }

  override def validate(input: String): Boolean | String = validateString(input)
}
