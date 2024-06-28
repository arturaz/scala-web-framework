package framework.prelude

import com.raquo.laminar.api.L.*
import org.scalajs.dom

import scala.util.control.NonFatal
import framework.utils.RouterOps

/** @note
  *   code taken from [[https://github.com/raquo/Waypoint#responding-to-link-clicks]].
  */
def navigateTo[BasePage, AppPage <: BasePage](page: AppPage)(using router: RouterOps[BasePage]): Binder[HtmlElement] =
  Binder { el =>
    val isLinkElement = el.ref.isInstanceOf[dom.html.Anchor]

    if (isLinkElement) {
      try {
        val url = router.absoluteUrlForPage(page)
        el.amend(href(url))
      } catch {
        case NonFatal(err) => dom.console.error(err)
      }
    }

    // If element is a link and user is holding a modifier while clicking:
    //  - Do nothing, browser will open the URL in new tab / window / etc. depending on the modifier key
    // Otherwise:
    //  - Perform regular pushState transition
    (
      onClick
        .filter(ev => !(isLinkElement && (ev.ctrlKey || ev.metaKey || ev.shiftKey || ev.altKey)))
        .preventDefault
        --> (_ => router.pushState(page))
    ).bind(el)
  }
