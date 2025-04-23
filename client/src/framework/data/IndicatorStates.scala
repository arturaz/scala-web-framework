package framework.data

import monocle.syntax.all.*
import alleycats.Empty

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

    /** Invoke this when you have received new data for the page.
      *
      * @param currentPage
      *   [[None]] if you are in no page (that is application is out of focus).
      */
    def newDataReceived(currentPage: Option[Page]): BooleanState[Page] =
      // Only show the indicator if we are not looking at the page
      if (currentPage.exists(forPages.matches)) this
      else copy(hasUnread = true)

    /** Invoke this when you have visited the page.
      *
      * @param currentPage
      *   [[None]] if you are in no page (that is application is out of focus).
      */
    def onPageSwitch(currentPage: Option[Page]): BooleanState[Page] =
      // Clear the indicator if we are looking at the page
      if (currentPage.exists(forPages.matches)) copy(hasUnread = false)
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
      *
      * @param currentPage
      *   [[None]] if you are in no page (that is application is out of focus).
      */
    def newDataReceived(currentPage: Option[Page], itemId: ItemId): SingleLevelItemsState[Page, ItemId] =
      newDataReceived(currentPage, itemIds = itemId :: Nil)

    /** Invoke this when new unread items is received.
      *
      * Nothing happens if you are currently looking at the page.
      *
      * @param currentPage
      *   [[None]] if you are in no page (that is application is out of focus).
      */
    def newDataReceived(currentPage: Option[Page], itemIds: IterableOnce[ItemId]): SingleLevelItemsState[Page, ItemId] =
      if (currentPage.exists(forPages.matches)) this
      else copy(unreadItems = unreadItems ++ itemIds)

    /** Invoke this when data for the page goes away (for example, someone else has processed the item). */
    def itemProcessed(itemId: ItemId): SingleLevelItemsState[Page, ItemId] =
      copy(unreadItems = unreadItems - itemId)

    /** Invoke this when data for the page goes away (for example, someone else has processed the items). */
    def itemsProcessed(itemIds: IterableOnce[ItemId]): SingleLevelItemsState[Page, ItemId] =
      copy(unreadItems = unreadItems -- itemIds)

    /** Invoke this when you have visited the page.
      *
      * @param currentPage
      *   [[None]] if you are in no page (that is application is out of focus).
      */
    def onPageSwitch(currentPage: Option[Page]): SingleLevelItemsState[Page, ItemId] =
      // Clear the indicator if we are looking at the page
      if (currentPage.exists(forPages.matches)) copy(unreadItems = Set.empty)
      else this

  }

  /** For pages which render a list of items, where you can enter into an item.
    *
    * An item is considered read when you enter it.
    *
    * @param unreadItems
    *   Set of unread item ids
    * @param currentlyIn
    *   The id of the item you are currently looking at
    */
  case class ListIndicatorState[ItemId](
    unreadItems: Set[ItemId],
    currentlyIn: Option[ItemId],
  ) extends State {
    def hasUnread: Boolean = unreadItems.nonEmpty

    def unreadCount: Int = unreadItems.size

    /** Invoke this when you have received new data for the specified items. */
    def newDataReceived(unreadItemIds: IterableOnce[ItemId]): ListIndicatorState[ItemId] =
      this
        .focus(_.unreadItems)
        .modify { items =>
          currentlyIn match {
            case None         => items ++ unreadItemIds
            case Some(itemId) => items ++ unreadItemIds - itemId
          }
        }

    /** Invoke this when you have visited the item. */
    def itemEntered(id: ItemId): ListIndicatorState[ItemId] = copy(
      unreadItems = unreadItems - id,
      currentlyIn = Some(id),
    )

    /** Invoke this when you have left the item. */
    def itemLeft: ListIndicatorState[ItemId] =
      copy(currentlyIn = None)

    /** Invoke this when you have processed the item and it does not need attention anymore. */
    def itemProcessed(id: ItemId): ListIndicatorState[ItemId] =
      copy(unreadItems = unreadItems - id)

    /** Invoke this when you have processed the items and it does not need attention anymore. */
    def itemsProcessed(itemIds: IterableOnce[ItemId]): ListIndicatorState[ItemId] =
      copy(unreadItems = unreadItems -- itemIds)
  }
  object ListIndicatorState {
    def empty[ItemId]: ListIndicatorState[ItemId] = apply(unreadItems = Set.empty, currentlyIn = None)
    given [ItemId]: Empty[ListIndicatorState[ItemId]] = Empty(empty)
  }
}
