package framework.utils

import yantl.Newtype
import cats.Show
import cats.syntax.show.*
import io.scalaland.chimney.Transformer
import cats.kernel.Order

trait NewtypeLong extends Newtype.Of[Long] {
  given Show[Type] = _.unwrap.show
  given CanEqual[Type, Type] = CanEqual.derived

  given Ordering[Type] = Ordering.by(unwrap)
  given Order[Type] = Order.fromOrdering

  given Conversion[Type, Long] = unwrap
  given Transformer[Type, Long] = unwrap

  def unapply(n: Type): Option[Long] = Some(n)
}
