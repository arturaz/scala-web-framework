package framework.exts

import cats.syntax.all.*
import framework.data.CookieNameFor
import org.scalajs.dom.document

extension [A](name: CookieNameFor[A]) {

  /** Deletes the HTTP cookie. */
  def deleteCookie(): Unit = {
    document.cookie = show"${name.name}=; Path=/; SameSite=Strict; expires=Thu, 01 Jan 1970 00:00:00 GMT"
  }
}
