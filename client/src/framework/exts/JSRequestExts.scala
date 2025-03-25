package framework.exts

import org.scalajs.dom.Request

extension (req: Request) {
  def asDebugString: String =
    s"${req.method} ${req.url} (cache=${req.cache}, mode=${req.mode}, redirect=${req.redirect}, mediaType=${req.mediaType})"
}
