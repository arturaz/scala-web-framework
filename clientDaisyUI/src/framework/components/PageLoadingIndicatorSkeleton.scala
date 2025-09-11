package framework.components

import com.raquo.laminar.api.L.*
import scala.concurrent.Future
import scala.concurrent.duration.*

object PageLoadingIndicatorSkeleton {
  case class ReloadButton(
    text: Signal[String],
    appearAfter: FiniteDuration = 3.seconds,
  )

  def apply(reloadButton: Option[ReloadButton]): PageLoadingIndicator = new {
    override def html =
      div(
        div(
          cls := "flex flex-col gap-4",
          div(cls := "skeleton h-32 w-72"),
          div(cls := "skeleton h-4 w-28"),
          div(cls := "skeleton h-4 w-full"),
          div(cls := "skeleton h-4 w-full"),
        ),
        div(
          cls := "flex justify-center md:justify-start",
          child.maybe <-- (reloadButton match {
            case Some(reloadButton) =>
              Signal.fromFuture(Future.delay(reloadButton.appearAfter)).mapSome { _ =>
                button(
                  cls := "btn btn-primary btn-wide mt-4",
                  child.text <-- reloadButton.text,
                  onClick --> { _ => window.location.reload() },
                )
              }

            case None => Signal.fromValue(None)
          }),
        ),
      )
  }
}
