package framework.components

import com.raquo.laminar.api.L.*
import framework.utils.PageRenderResult
import scala.concurrent.Future
import scala.concurrent.duration.*

object PageLoadingIndicatorSkeleton extends PageLoadingIndicator {
  case class ReloadButton(
    text: Signal[String],
    appearAfter: FiniteDuration = 3.seconds,
  )

  override def html: HtmlElement = html(reloadButton = None).html

  def html(reloadButton: Option[ReloadButton] = None): PageLoadingIndicator = new {
    override def html =
      div(
        cls := "flex flex-col gap-4",
        div(cls := "skeleton h-32 w-72"),
        div(cls := "skeleton h-4 w-28"),
        div(cls := "skeleton h-4 w-full"),
        div(cls := "skeleton h-4 w-full"),
        child.maybe <-- (reloadButton match {
          case Some(reloadButton) =>
            Signal.fromFuture(Future.delay(reloadButton.appearAfter)).mapSome { _ =>
              // TODO: render a button.
              div()
            }

          case None => Signal.fromValue(None)
        }),
      )
  }
}
