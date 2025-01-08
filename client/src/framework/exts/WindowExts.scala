package framework.exts

import org.scalajs.dom.Window

extension (window: Window) {

  /** Scrolls to the top of the page. */
  def scrollToTop(): Unit = {
    window.scrollTo(window.scrollX.round.toInt, 0)
  }

  /** Scrolls to the bottom of the page. */
  def scrollToBottom(): Unit = {
    window.scrollTo(window.scrollX.round.toInt, window.document.body.scrollHeight)
  }
}
