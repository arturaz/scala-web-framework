package framework.components

import com.raquo.laminar.api.L.*
import framework.utils.PageRenderResult

object PageLoadingIndicator {
  def indicator: HtmlElement =
    div(
      cls := "flex flex-col gap-4",
      div(cls := "skeleton h-32 w-72"),
      div(cls := "skeleton h-4 w-28"),
      div(cls := "skeleton h-4 w-full"),
      div(cls := "skeleton h-4 w-full"),
    )

  def renderResult: PageRenderResult = PageRenderResult.fromElement(indicator)
}
