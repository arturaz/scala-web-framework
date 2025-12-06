package framework.prelude

import sttp.model.Uri
import framework.exts.fromKeyCodecs

// Prefix types with `Circe` to avoid confusion with `tapir`s `Codec`.
export io.circe.{
  Codec as CirceCodec,
  Decoder as CirceDecoder,
  Encoder as CirceEncoder,
  KeyDecoder as CirceKeyDecoder,
  KeyEncoder as CirceKeyEncoder,
}
export framework.utils.CirceKeyCodec

export io.circe.syntax.{EncoderOps, KeyOps}
export io.circe.parser.{decode as decodeJson, decodeAccumulating as decodeJsonAccumulating, parse as parseJson}

// Export faster jsoniter-based circe codecs to default scope
export com.github.plokhotnyuk.jsoniter_scala.circe.CirceCodecs.*

given CirceKeyEncoder[Uri] = CirceKeyEncoder.instance(_.toString)
given CirceKeyDecoder[Uri] = CirceKeyDecoder.instance(Uri.parse(_).toOption)

given CirceCodec[Uri] = CirceCodec.fromKeyCodecs
