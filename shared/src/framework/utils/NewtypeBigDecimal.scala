package framework.utils

import cats.Show
import neotype.Newtype
import io.scalaland.chimney.Transformer
import framework.exts.*

/** A newtype wrapping a [[BigDecimal]]. Does not support validations. */
trait NewtypeBigDecimal extends Newtype[BigDecimal] {
  val zero: Type = make(BigDecimal(0)).getOrThrow

  given Show[Type] = unwrap(_).toString()

  given Ordering[Type] = Ordering.by(unwrap)

  given Transformer[Type, BigDecimal] = unwrap
  given Transformer[BigDecimal, Type] = make(_).getOrThrow
  given CanEqual[Type, Type] = CanEqual.derived
}
