package framework.utils

import cats.Show
import yantl.Newtype
import io.scalaland.chimney.Transformer
import framework.exts.*

/** A newtype wrapping a [[BigDecimal]]. Does not support validations. */
trait NewtypeBigDecimal extends Newtype.WithoutValidationOf[BigDecimal] {
  val zero: Type = apply(BigDecimal(0))

  given Show[Type] = unwrap(_).toString()

  given Ordering[Type] = Ordering.by(unwrap)

  given Transformer[Type, BigDecimal] = unwrap
  given Transformer[BigDecimal, Type] = make(_).getOrThrow
  given CanEqual[Type, Type] = CanEqual.derived
}
