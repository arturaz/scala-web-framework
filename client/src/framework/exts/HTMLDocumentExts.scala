package framework.exts

extension (doc: org.scalajs.dom.HTMLDocument) {

  /** Returns the cookies as a [[Map]]. */
  def cookieMap: Map[String, String] =
    doc.cookie
      .split(';')
      .iterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.split("=", 2))
      .map { case Array(k, v) => (k, v) }
      .toMap
}
