package framework.prelude

import com.raquo.laminar.api.L.*
import org.scalajs.dom

import scala.util.control.NonFatal
import framework.utils.RouterOps
import com.raquo.waypoint.Router
import com.raquo.laminar.modifiers.EventListener
import org.scalajs.dom.MouseEvent
import scala.util.chaining.*
import scala.annotation.targetName

/** Navigates to the given page.
  *
  * @note
  *   code taken from [[https://github.com/raquo/Waypoint#responding-to-link-clicks]].
  */
def navigateTo[BasePage, AppPage <: BasePage](signal: Signal[AppPage])(using
  router: RouterOps[BasePage]
): Binder[HtmlElement] =
  navigateTo(signal.map(Some.apply))

@targetName("navigateToOption")
def navigateTo[BasePage, AppPage <: BasePage](signal: Signal[Option[AppPage]])(using
  router: RouterOps[BasePage]
): Binder[HtmlElement] =
  Binder { el =>
    val isLinkElement = el.ref.isInstanceOf[dom.html.Anchor]

    if (isLinkElement) {
      try {
        val urlSignal = signal.map(_.fold("")(router.absoluteUrlForPage))
        el.amend(href <-- urlSignal)
      } catch {
        case NonFatal(err) => logError(s"navigateTo: $err")
      }
    }

    val binder = navigateToEventProcessor(isLinkElement).compose(_.sample(signal)) --> {
      case None       => ()
      case Some(page) => router.pushState(page)
    }
    binder.bind(el)
  }

def navigateTo[BasePage, AppPage <: BasePage](page: AppPage)(using
  router: RouterOps[BasePage]
): Binder[HtmlElement] =
  navigateTo(Signal.fromValue(page))

def navigateToEventProcessor(
  isLinkElement: Boolean
): EventProcessor[MouseEvent, MouseEvent] = {
  // If element is a link and user is holding a modifier while clicking:
  //  - Do nothing, browser will open the URL in new tab / window / etc. depending on the modifier key
  // Otherwise:
  //  - Perform regular pushState transition
  onClick
    .filter(ev => !(isLinkElement && (ev.ctrlKey || ev.metaKey || ev.shiftKey || ev.altKey)))
    .preventDefault
}
