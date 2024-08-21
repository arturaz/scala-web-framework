package framework.prelude

import sttp.model.Uri
import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.Transformer

given Show[Uri] = _.toString

given PartialTransformer[String, Uri] = PartialTransformer(str => Result.fromEitherString(Uri.parse(str)))
given Transformer[Uri, String] = _.toString
