package framework.prelude

import org.http4s.{Method, Uri}
import org.http4s.MediaRange
import org.http4s.MediaType

given CanEqual[Method, Method] = CanEqual.derived
given CanEqual[Uri.Path, Uri.Path] = CanEqual.derived
given CanEqual[MediaRange, MediaType] = CanEqual.derived

given sttpToHttp4sUri: Conversion[sttp.model.Uri, Uri] = uri => Uri.unsafeFromString(uri.toString)
given http4sToSttpUri: Conversion[Uri, sttp.model.Uri] = uri => sttp.model.Uri.parse(uri.toString).getOrThrow

given CirceEncoder[Uri.Host] = _.renderString.asJson
