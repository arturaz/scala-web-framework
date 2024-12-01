package framework.exts

import framework.prelude.*

extension (obj: TapirCodec.type) {

  /** Convenience method to create a JSON codec. */
  def fromJsonCodec[A: CirceEncoder: CirceDecoder: Schema]: TapirCodec[String, A, TapirCodecFormat.Json] =
    sttp.tapir.json.circe.circeCodec
}
