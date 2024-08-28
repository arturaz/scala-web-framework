package framework.utils

import neotype.Newtype
import cats.Show
import cats.syntax.show.*
import io.scalaland.chimney.Transformer
import cats.kernel.Order

trait NewtypeInt extends Newtype[Int] {
  given Show[Type] = unwrap(_).show
  given CanEqual[Type, Type] = CanEqual.derived

  given Ordering[Type] = Ordering.by(unwrap)
  given Order[Type] = Order.fromOrdering

  given Conversion[Type, Int] = unwrap
  given Transformer[Type, Int] = unwrap
}
