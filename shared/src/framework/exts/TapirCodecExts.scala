package framework.exts

import framework.prelude.*
import sttp.tapir.DecodeResult

import java.nio.charset.{Charset, StandardCharsets}
import java.util.Base64
import scala.util.Try
import scodec.Attempt.Successful
import scodec.Attempt.Failure
import scodec.bits.Bases.Alphabets.Base64UrlNoPad
import scodec.bits.BitVector
import sttp.tapir.CodecFormat.TextPlain

extension (obj: TapirCodec.type) {

  /** Convenience method to create a JSON codec. */
  def fromJsonCodec[A: CirceEncoder: CirceDecoder: Schema]: TapirCodec[String, A, TapirCodecFormat.Json] =
    sttp.tapir.json.circe.circeCodec

  /** Codec using `scodec` and base64. */
  def fromSCodecAsBase64[A](using
    codec: SCodecCodec[A],
    schema: Schema[A],
  ): TapirCodec[String, A, TapirCodecFormat.TextPlain] = {
    new TapirCodec[String, A, TapirCodecFormat.TextPlain] {
      override def encode(h: A): String = codec.encode(h) match {
        case Successful(byteVector) => byteVector.toBase64UrlNoPad
        case Failure(cause)         => throw new IllegalArgumentException(cause.messageWithContext)
      }

      override def rawDecode(l: String): DecodeResult[A] =
        DecodeResult
          .fromEitherString(
            l,
            BitVector
              .fromBase64Descriptive(l, Base64UrlNoPad)
              .flatMap(codec.decodeValue(_).toEither.left.map(_.messageWithContext)),
          )

      override val format: TapirCodecFormat.TextPlain = TapirCodecFormat.TextPlain()

      override def schema: Schema[A] = summon
    }
  }
}

extension [LowLevel, HighLevel, CodecFormat <: TapirCodecFormat](codec: TapirCodec[LowLevel, HighLevel, CodecFormat]) {

  /** Maps the input type of the [[TapirCodec]]. */
  def mapInput[LowLevel2](
    encodeInput: LowLevel => LowLevel2
  )(decodeInput: LowLevel2 => DecodeResult[LowLevel]): TapirCodec[LowLevel2, HighLevel, CodecFormat] = {
    new TapirCodec[LowLevel2, HighLevel, CodecFormat] {
      override def rawDecode(l: LowLevel2): DecodeResult[HighLevel] = decodeInput(l).flatMap(codec.decode)
      override def encode(h: HighLevel): LowLevel2 = encodeInput(codec.encode(h))
      override def format: CodecFormat = codec.format
      override def schema: Schema[HighLevel] = codec.schema
    }
  }
}

extension [HighLevel, CodecFormat <: TapirCodecFormat](codec: TapirCodec[String, HighLevel, CodecFormat]) {

  /** Encodes the input as base64. */
  def base64ed(charset: Charset = StandardCharsets.UTF_8): TapirCodec[String, HighLevel, CodecFormat] = {
    codec.mapInput(rawString => Base64.getUrlEncoder().encodeToString(rawString.getBytes(charset))) { base64String =>
      val either = Try(new String(Base64.getUrlDecoder().decode(base64String), charset)).toEither.left.map(err =>
        show"Failed to decode as base64: ${err.getMessage}"
      )
      DecodeResult.fromEitherString(base64String, either)
    }
  }
}

extension [A](dr: DecodeResult[A]) {
  def toEither: Either[DecodeResult.Failure, A] = dr match {
    case DecodeResult.Value(value)     => Right(value)
    case failure: DecodeResult.Failure => Left(failure)
  }
}
