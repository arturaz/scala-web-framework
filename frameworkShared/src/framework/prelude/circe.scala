package framework.prelude

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
