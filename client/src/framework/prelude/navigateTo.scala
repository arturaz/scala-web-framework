package framework.prelude

import com.raquo.laminar.api.L.*
import com.raquo.laminar.modifiers.EventListener
import com.raquo.waypoint.Router
import framework.utils.RouterOps
import org.scalajs.dom
import org.scalajs.dom.MouseEvent

import scala.annotation.targetName
import scala.util.chaining.*
import scala.util.control.NonFatal

/** Navigates to the given page.
  *
  * @note
  *   code taken from [[https://github.com/raquo/Waypoint#responding-to-link-clicks]].
  */
def navigateTo[BasePage, AppPage <: BasePage](signal: Signal[AppPage])(using
  router: RouterOps.In[BasePage]
): Binder[HtmlElement] =
  navigateTo(signal.map(Some.apply))

@targetName("navigateToOption")
def navigateTo[BasePage, AppPage <: BasePage](signal: Signal[Option[AppPage]])(using
  router: RouterOps.In[BasePage]
): Binder[HtmlElement] =
  Binder { el =>
    val isLinkElement = el.ref.isInstanceOf[dom.html.Anchor]

    if (isLinkElement) {
      try {
        val urlSignal = signal.map(_.fold("")(router.absoluteUrlForPage))
        el.amend(href <-- urlSignal)
      } catch {
        case NonFatal(err) => log.error(s"navigateTo: $err")
      }
    }

    val binder = navigateToEventProcessor(isLinkElement).compose(_.sample(signal)) --> {
      case None       => ()
      case Some(page) => router.pushState(page)
    }
    binder.bind(el)
  }

def navigateTo[BasePage, AppPage <: BasePage](page: AppPage)(using
  router: RouterOps.In[BasePage]
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
