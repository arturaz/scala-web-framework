package framework.prelude

import neotype.Newtype
import sttp.model.Uri
import ciris.ConfigError

export framework.config.EnvConfigPrefix
export ciris.{ConfigDecoder, ConfigValue}
export ciris.http4s.{hostConfigDecoder, portConfigDecoder, uriConfigDecoder}
export ciris.squants.stringQuantityConfigDecoder

given ConfigDecoder[String, Uri] =
  ConfigDecoder.instance((key, str: String) => Uri.parse(str).left.map(err => ConfigError(err)))

given configDecoderForUriWrapper[Wrapper](using newtype: Newtype.WithType[Uri, Wrapper]): ConfigDecoder[Uri, Wrapper] =
  ConfigDecoder[Uri].map(newtype.make(_).getOrThrow)

given configDecoderForUriWrapperFromString[Wrapper](using
  newtype: Newtype.WithType[Uri, Wrapper]
): ConfigDecoder[String, Wrapper] =
  ConfigDecoder[String, Uri].as[Wrapper]
