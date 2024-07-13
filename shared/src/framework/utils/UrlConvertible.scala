package framework.utils

import sttp.tapir.{Codec, CodecFormat, DecodeResult}
import urldsl.errors.ErrorFromThrowable
import urldsl.vocabulary.{FromString, Printer}

/** Convenience trait that combines [[FromString]] and [[Printer]]. */
trait UrlConvertible[Result, Err] extends FromString[Result, Err] with Printer[Result]
object UrlConvertible {
  def apply[Result, Err](
    fromString: String => Either[Err, Result],
    toString: Result => String,
  ): UrlConvertible[Result, Err] = {
    val parse = fromString
    val render = toString

    new UrlConvertible[Result, Err] {
      override def fromString(str: String): Either[Err, Result] = parse(str)
      override def print(t: Result): String = render(t)
    }
  }

  /** Example:
    * {{{
    *   given codec: Codec[String, Wrapped, CodecFormat.TextPlain] = ???
    *   given urlConvertible: UrlConvertible[Wrapped, urldsl.errors.DummyError] = UrlConvertible.fromCodec
    * }}}
    */
  given fromCodec[A, Err](using
    codec: Codec[String, A, CodecFormat.TextPlain],
    errorFromThrowable: ErrorFromThrowable[Err],
  ): UrlConvertible[A, Err] with {
    override def fromString(str: String): Either[Err, A] = codec.decode(str) match {
      case failure: DecodeResult.Failure => Left(errorFromThrowable.fromThrowable(new Exception(failure.toString)))
      case DecodeResult.Value(v)         => Right(v)
    }

    override def print(t: A): String = codec.encode(t)
  }
}
