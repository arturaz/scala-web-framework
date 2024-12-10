package framework.utils

import yantl.Newtype
import cats.Show
import cats.syntax.show.*
import io.scalaland.chimney.Transformer
import cats.kernel.Order

trait NewtypeDouble extends Newtype.Of[Double] {
  given Show[Type] = unwrap(_).show
  given CanEqual[Type, Type] = CanEqual.derived

  given Ordering[Type] = Ordering.by(unwrap)
  given Order[Type] = Order.fromOrdering

  given Conversion[Type, Double] = unwrap
  given Transformer[Type, Double] = unwrap

  extension (d: Type) {
    def toDouble: Double = unwrap(d)
  }
}
