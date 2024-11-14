package framework.components

import framework.utils.PageRenderResult

/** An indicator that is shown when page content is loading. */
trait PageLoadingIndicator {
  def html: L.HtmlElement

  def pageRenderResult: PageRenderResult = PageRenderResult.fromElement(html)
}
