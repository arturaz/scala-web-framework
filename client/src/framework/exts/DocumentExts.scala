package framework.exts

import org.scalajs.dom.Document

extension (document: Document) {

  /** Gets the first <meta> element with the given name and returns its `content` attribute. */
  def getMetaContent(name: String): Option[String] = {
    Option(document.querySelector(show"""meta[name="$name"]""")).map(e => e.getAttribute("content"))
  }
}
