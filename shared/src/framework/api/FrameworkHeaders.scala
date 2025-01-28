package framework.api

import cats.syntax.show.*
import framework.data.FrameworkDateTime

object FrameworkHeaders {

  /** A unix timestamp when the request was started in the client. */
  object `X-Request-Started-At` {
    final val Name = "X-Request-Started-At"

    /** Returns a (headerName, value) pair. */
    def apply(now: FrameworkDateTime): (String, String) =
      (Name, now.toUnixMillis.show)
  }
}
