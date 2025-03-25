package framework.exts

import org.scalajs.dom.Request

extension (req: Request) {
  def asDebugString: String =
    s"${req.method} ${req.url} (mediaType=${req.mediaType}, cache=${req.cache}, mode=${req.mode}, redirect=${req.redirect})"
}
