package framework.utils

import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L

/** Helper to dynamically compose the content of a page and the page title. */
case class PageRenderResult(
  content: Signal[L.Element],
  pageTitle: Signal[PageTitleResult] = Signal.fromValue(PageTitleResult.default),
)
object PageRenderResult {
  given Conversion[L.Element, PageRenderResult] = fromElement(_)

  def fromElement(
    content: L.Element,
    pageTitle: Signal[PageTitleResult] = Signal.fromValue(PageTitleResult.default),
  ): PageRenderResult = apply(Signal.fromValue(content), pageTitle)

  extension (signal: Signal[PageRenderResult]) {

    /** Turns a [[Signal]] of [[PageRenderResult]] into a [[PageRenderResult]]. */
    def extract: PageRenderResult = {
      val content = signal.flatMapSwitch(_.content)
      val pageTitle = signal.flatMapSwitch(_.pageTitle)

      PageRenderResult(content, pageTitle)
    }
  }
}
