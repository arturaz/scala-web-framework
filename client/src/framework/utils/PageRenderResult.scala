package framework.utils

import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L
import framework.exts.*
import framework.data.MaybeSignal

/** Helper to dynamically compose the content of a page and the page title. */
case class PageRenderResult(
  content: MaybeSignal[L.Element],
  pageTitle: MaybeSignal[PageTitleResult] = PageTitleResult.default,
)
object PageRenderResult {
  given Conversion[L.Element, PageRenderResult] = fromElement(_)

  def fromElement(
    content: L.Element,
    pageTitle: MaybeSignal[PageTitleResult] = PageTitleResult.default,
  ): PageRenderResult = apply(content, pageTitle)

  extension (signal: Signal[PageRenderResult]) {

    /** Turns a [[Signal]] of [[PageRenderResult]] into a [[PageRenderResult]]. */
    def extract: PageRenderResult = {
      val content = signal.flatMapSwitch(_.content.deunionizeSignal)
      val pageTitle = signal.flatMapSwitch(_.pageTitle.deunionizeSignal)

      PageRenderResult(content, pageTitle)
    }
  }
}
