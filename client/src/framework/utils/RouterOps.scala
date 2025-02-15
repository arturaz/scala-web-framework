package framework.utils

import com.raquo.waypoint.Router
import com.raquo.airstream.state.StrictSignal

/** A trait that provides access to the router. */
trait RouterOps[BasePage] extends RouterOps.Out[BasePage] with RouterOps.In[BasePage]
object RouterOps {

  /** Covariant trait for the output side. */
  trait Out[+BasePage] {

    /** @see [[Router.currentPageSignal]]. */
    def currentPageSignal: StrictSignal[BasePage]

    /** @see [[Router.pageForAbsoluteUrl]]. */
    def pageForAbsoluteUrl(url: String): Option[BasePage]

    /** @see [[Router.pageForRelativeUrl]]. */
    def pageForRelativeUrl(url: String): Option[BasePage]
  }

  /** Contravariant trait for the input side. */
  trait In[-BasePage] {

    /** @see [[Router.absoluteUrlForPage]]. */
    def absoluteUrlForPage(page: BasePage): String

    /** @see [[Router.relativeUrlForPage]]. */
    def relativeUrlForPage(page: BasePage): String

    /** @see [[Router.pushState]]. */
    def pushState(page: BasePage): Unit

    /** @see [[Router.replaceState]]. */
    def replaceState(page: BasePage): Unit

    /** @see [[Router.forcePage]]. */
    def forcePage(page: BasePage): Unit
  }

  def fromRouter[BasePage](router: Router[BasePage]): RouterOps[BasePage] = new RouterOps[BasePage] {
    override def currentPageSignal: StrictSignal[BasePage] = router.currentPageSignal
    override def pageForAbsoluteUrl(url: String): Option[BasePage] = router.pageForAbsoluteUrl(url)
    override def pageForRelativeUrl(url: String): Option[BasePage] = router.pageForRelativeUrl(url)
    override def absoluteUrlForPage(page: BasePage): String = router.absoluteUrlForPage(page)
    override def relativeUrlForPage(page: BasePage): String = router.relativeUrlForPage(page)
    override def pushState(page: BasePage): Unit = router.pushState(page)
    override def replaceState(page: BasePage): Unit = router.replaceState(page)
    override def forcePage(page: BasePage): Unit = router.forcePage(page)
  }
}
