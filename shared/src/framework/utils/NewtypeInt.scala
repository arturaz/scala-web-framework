package framework.utils

import yantl.Newtype
import cats.Show
import cats.syntax.show.*
import io.scalaland.chimney.Transformer
import cats.kernel.Order

trait NewtypeInt extends Newtype.Of[Int] {
  given Show[Type] = _.unwrap.show
  given CanEqual[Type, Type] = CanEqual.derived

  given Ordering[Type] = Ordering.by(unwrap)
  given Order[Type] = Order.fromOrdering

  given Conversion[Type, Int] = unwrap
  given Transformer[Type, Int] = unwrap
}
