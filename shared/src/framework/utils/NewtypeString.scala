package framework.utils

import cats.Show
import yantl.Newtype
import io.scalaland.chimney.Transformer
import framework.exts.*
import framework.prelude.{*, given}
import cats.kernel.Order

/** A newtype wrapping a [[String]]. */
trait NewtypeString extends Newtype.Of[String] {
  given Show[Type] = unwrap(_)
  given CanEqual[Type, Type] = CanEqual.derived

  given Ordering[Type] = Ordering.by(unwrap)
  given Order[Type] = Order.fromOrdering

  given tapirCodec: TapirCodec[String, Type, TapirCodecFormat.TextPlain] =
    TapirCodec.string.mapEither(make.asString)(unwrap)

  given Transformer[Type, String] = unwrap
}
