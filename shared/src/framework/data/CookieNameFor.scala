package framework.data

import cats.Show

/** Specifies the name to use when setting the value `A` as a HTTP cookie. */
case class CookieNameFor[A](name: String)
object CookieNameFor {
  given Show[CookieNameFor[?]] = _.name
}
