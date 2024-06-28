package framework.utils

import cats.Show
import neotype.Newtype
import io.scalaland.chimney.Transformer
import framework.exts.*

/** A newtype wrapping a [[String]]. */
trait NewtypeString extends Newtype[String] {
  given Show[Type] = unwrap(_)
  given CanEqual[Type, Type] = CanEqual.derived

  given Ordering[Type] = Ordering.by(unwrap)

  given Transformer[Type, String] = unwrap
  given Transformer[String, Type] = make(_).getOrThrow
}
