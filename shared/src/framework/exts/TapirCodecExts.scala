package framework.exts

import framework.prelude.*
import sttp.tapir.DecodeResult

extension (obj: TapirCodec.type) {

  /** Convenience method to create a JSON codec. */
  def fromJsonCodec[A: CirceEncoder: CirceDecoder: Schema]: TapirCodec[String, A, TapirCodecFormat.Json] =
    sttp.tapir.json.circe.circeCodec
}

extension [A](dr: DecodeResult[A]) {
  def toEither: Either[DecodeResult.Failure, A] = dr match {
    case DecodeResult.Value(value)     => Right(value)
    case failure: DecodeResult.Failure => Left(failure)
  }
}
