package framework.utils

import cats.Show
import cats.syntax.show.*
import neotype.Newtype
import io.scalaland.chimney.Transformer
import framework.exts.*

/** A newtype wrapping a [[Boolean]]. */
trait NewtypeBoolean extends Newtype[Boolean] {
  given Show[Type] = unwrap(_).show
  given CanEqual[Type, Type] = CanEqual.derived

  given Ordering[Type] = Ordering.by(unwrap)

  given Conversion[Type, Boolean] = unwrap
  given Transformer[Type, Boolean] = unwrap
  given Transformer[Boolean, Type] = make(_).getOrThrow
}
