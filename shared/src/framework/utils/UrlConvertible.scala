package framework.utils

import sttp.tapir.{Codec, CodecFormat, DecodeResult}
import urldsl.errors.ErrorFromThrowable
import urldsl.vocabulary.{FromString, Printer}

/** Convenience trait that combines [[FromString]] and [[Printer]]. */
trait UrlConvertible[Result, Err] extends FromString[Result, Err] with Printer[Result]
object UrlConvertible {
  implicit def fromCodec[A, Err](implicit
    codec: Codec[String, A, CodecFormat.TextPlain],
    errorFromThrowable: ErrorFromThrowable[Err],
  ): UrlConvertible[A, Err] = {
    new UrlConvertible[A, Err] {
      override def fromString(str: String): Either[Err, A] = codec.decode(str) match {
        case failure: DecodeResult.Failure => Left(errorFromThrowable.fromThrowable(new Exception(failure.toString)))
        case DecodeResult.Value(v)         => Right(v)
      }

      override def print(t: A): String = codec.encode(t)
    }
  }
}
