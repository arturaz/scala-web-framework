package framework.data

/** Various data structures that are used for indicators that show the state of a page.
  *
  * Think of tiny little badges next to links so you would know if there's data there before following a link and
  * opening that page.
  */
object IndicatorStates {
  type PageMatcher[Page] = Set[Page] | PartialFunction[Page, Unit]
  extension [Page](matcher: PageMatcher[Page]) {
    def matches(page: Page): Boolean = matcher match {
      case set: Set[Page @unchecked]                             => set.contains(page)
      case pf: PartialFunction[Page @unchecked, Unit @unchecked] => pf.isDefinedAt(page)
    }
  }

  /** Trait for states that track unread data. */
  trait State {

    /** Whether there is any unread data. */
    def hasUnread: Boolean

    /** Number of unread items. */
    def unreadCount: Int
  }

  /** Simple boolean state for pages that have no hierarchy. Indicator is set only if you are not looking at the page.
    * Once you visit the page, the indicator is cleared.
    *
    * @param forPages
    *   matches if this is a page that is indicated by this indicator
    */
  case class BooleanState[Page](forPages: PageMatcher[Page], hasUnread: Boolean) extends State {
    def unreadCount: Int = if (hasUnread) 1 else 0

    /** Invoke this when you have received new data for the page. */
    def newDataReceived(currentPage: Page): BooleanState[Page] =
      // Only show the indicator if we are not looking at the page
      if (forPages.matches(currentPage)) this
      else copy(hasUnread = true)

    /** Invoke this when you have visited the page. */
    def onPageSwitch(currentPage: Page): BooleanState[Page] =
      // Clear the indicator if we are looking at the page
      if (forPages.matches(currentPage)) copy(hasUnread = false)
      else this
  }

  /** For pages which render a list of items at one level (you can't enter the items and all of the items are considered
    * seen once you enter the page).
    *
    * @param forPages
    *   matches if this is a page that is indicated by this indicator
    * @param unreadItems
    *   set of unread item ids
    */
  case class SingleLevelItemsState[Page, ItemId](forPages: PageMatcher[Page], unreadItems: Set[ItemId]) extends State {
    def hasUnread: Boolean = unreadItems.nonEmpty

    def unreadCount: Int = unreadItems.size

    /** Invoke this when new unread item is received.
      *
      * Nothing happens if you are currently looking at the page.
      */
    def newDataReceived(currentPage: Page, itemId: ItemId): SingleLevelItemsState[Page, ItemId] =
      newDataReceived(currentPage, itemIds = itemId :: Nil)

    /** Invoke this when new unread items is received.
      *
      * Nothing happens if you are currently looking at the page.
      */
    def newDataReceived(currentPage: Page, itemIds: IterableOnce[ItemId]): SingleLevelItemsState[Page, ItemId] =
      if (forPages.matches(currentPage)) this
      else copy(unreadItems = unreadItems ++ itemIds)

    /** Invoke this when data for the page goes away (for example, someone else has processed the item). */
    def itemProcessed(itemId: ItemId): SingleLevelItemsState[Page, ItemId] =
      copy(unreadItems = unreadItems - itemId)

    /** Invoke this when you have visited the page. */
    def onPageSwitch(currentPage: Page): SingleLevelItemsState[Page, ItemId] =
      // Clear the indicator if we are looking at the page
      if (forPages.matches(currentPage)) copy(unreadItems = Set.empty)
      else this

  }
}
