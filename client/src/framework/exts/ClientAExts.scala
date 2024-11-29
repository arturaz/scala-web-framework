package framework.exts

import cats.syntax.all.*
import framework.data.CookieNameFor
import org.scalajs.dom.document
import sttp.tapir.{Codec, CodecFormat}

extension [A](a: A) {

  /** Sets this value as a HTTP cookie. */
  def setAsCookie()(using
    codec: Codec[String, A, CodecFormat.TextPlain],
    name: CookieNameFor[A],
  ): Either[String, Unit] = {
    val encoded = codec.encode(a)

    document.cookie = show"$name=$encoded; Path=/; SameSite=Strict"
    document.cookieMap.get(name.name) match {
      case None            => Left(show"Failed to set cookie: ${name}")
      case Some(`encoded`) => Right(())
      case Some(other)     => Left(show"Failed to set cookie: ${name}, got '$other', expected '$encoded'")
    }
  }
}
