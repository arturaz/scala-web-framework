package framework.prelude

import ciris.ConfigError
import framework.data.{Email, FrameworkDate, FrameworkDateTime}
import org.tpolecat.typename.TypeName
import scribe.Level
import sttp.model.Uri
import yantl.Newtype

import java.nio.file.Path

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
given ConfigDecoder[String, FrameworkDate] =
  ConfigDecoder.instance((key, str: String) =>
    FrameworkDate.fromString(str).left.map(err => ConfigError(show"Invalid date '$str': $err"))
  )

given configDecoderForUriWrapper[Wrapper](using newtype: Newtype.WithType[Uri, Wrapper]): ConfigDecoder[Uri, Wrapper] =
  ConfigDecoder[Uri].map(newtype.make(_).getOrThrow)

given configDecoderForUriWrapperFromString[Wrapper](using
  newtype: Newtype.WithType[Uri, Wrapper]
): ConfigDecoder[String, Wrapper] =
  ConfigDecoder[String, Uri].as[Wrapper]

given configDecoderForEmailValidator: ConfigDecoder[String, Email.Validator] =
  summon[ConfigDecoder[String, String]].mapEither { (maybeKey, str) =>
    Email.Validator
      .fromString(str)
      .left
      .map(_ => ConfigError.decode(typeName = summon[TypeName[Email.Validator]].value, key = maybeKey, value = str))
  }
