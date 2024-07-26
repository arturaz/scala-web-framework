package framework.utils

import neotype.Newtype
import sttp.model.Uri
import framework.prelude.*

trait NewtypeUri extends Newtype[Uri] {
  given Conversion[Type, Uri] = unwrap
  given Show[Type] = unwrap(_).toString
}
