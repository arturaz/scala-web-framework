package framework.config

import framework.utils.{HasRoute, NewtypeUri}
import sttp.model.Uri

/** An [[Uri]] that points to our frontend. */
type FrontendUri = FrontendUri.Type
object FrontendUri extends NewtypeUri {
  extension (v: Type) {
    def uriForPage[Page](page: Page)(using hasRoute: HasRoute[Page]): Uri = {
      val uri = unwrap(v)
      uri.withWholePath(hasRoute.route.relativeUrlForPage(page))
    }
  }
}
