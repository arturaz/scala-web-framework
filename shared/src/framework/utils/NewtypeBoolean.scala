package framework.utils

import cats.Show
import cats.syntax.show.*
import framework.exts.*
import io.scalaland.chimney.Transformer
import yantl.Newtype

/** A newtype wrapping a [[Boolean]]. */
trait NewtypeBoolean extends Newtype.WithoutValidationOf[Boolean] {
  given Show[Type] = _.unwrap.show
  given CanEqual[Type, Type] = CanEqual.derived

  given Ordering[Type] = Ordering.by(unwrap)

  given Conversion[Type, Boolean] = unwrap
  given Transformer[Type, Boolean] = unwrap
  given Transformer[Boolean, Type] = apply

  /** Pattern match extractor for this newtype. */
  def unapply(b: Type): Option[Boolean] = Some(b)
}
