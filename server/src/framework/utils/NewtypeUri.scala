package framework.utils

import neotype.Newtype
import org.http4s.Uri

trait NewtypeUri extends Newtype[Uri] {
  given Conversion[Type, Uri] = unwrap
  given ConfigDecoder[Uri, Type] = ConfigDecoder[Uri].map(make(_).getOrThrow)
  given ConfigDecoder[String, Type] = ConfigDecoder[String, Uri].as[Type]
  given Show[Type] = unwrap(_).show
}
