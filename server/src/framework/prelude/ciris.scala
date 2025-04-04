package framework.prelude

import yantl.Newtype
import sttp.model.Uri
import ciris.ConfigError
import scribe.Level
import java.nio.file.Path
import framework.data.FrameworkDateTime

export framework.config.EnvConfigPrefix
export ciris.{ConfigDecoder, ConfigValue}
export ciris.http4s.{hostConfigDecoder, portConfigDecoder, uriConfigDecoder}

given ConfigDecoder[String, Uri] =
  ConfigDecoder.instance((key, str: String) => Uri.parse(str).left.map(err => ConfigError(err)))

given ConfigDecoder[String, Level] =
  ConfigDecoder.instance((key, str: String) => Level.get(str).toRight(ConfigError(show"Invalid log level: $str")))

given ConfigDecoder[String, Path] =
  ConfigDecoder.instance((key, str: String) => Right(Path.of(str)))

given ConfigDecoder[String, FrameworkDateTime] =
  ConfigDecoder.instance((key, str: String) =>
    FrameworkDateTime.fromString(str).left.map(err => ConfigError(show"Invalid date time '$str': $err"))
  )

given configDecoderForUriWrapper[Wrapper](using newtype: Newtype.WithType[Uri, Wrapper]): ConfigDecoder[Uri, Wrapper] =
  ConfigDecoder[Uri].map(newtype.make(_).getOrThrow)

given configDecoderForUriWrapperFromString[Wrapper](using
  newtype: Newtype.WithType[Uri, Wrapper]
): ConfigDecoder[String, Wrapper] =
  ConfigDecoder[String, Uri].as[Wrapper]
