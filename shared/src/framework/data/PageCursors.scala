package framework.data

import monocle.*

/** Page cursors that have been bundled with an input.
  *
  * @param currentPage
  *   the input for the current page
  * @param previousPageCursor
  *   the applicable cursor for the previous page. Note that this cursor is only valid for the `currentPage`.
  * @param nextPageCursor
  *   the applicable cursor for the next page. Note that this cursor is only valid for the `currentPage`.
  */
case class PageCursors[Input, CursorPrimaryColumn, CursorSecondaryColumn, PageSize](
  lens: PageCursor.Lens[Input, CursorPrimaryColumn, CursorSecondaryColumn, PageSize],
  currentPage: Input,
  previousPageCursor: Option[PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize]],
  nextPageCursor: Option[PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize]],
) extends PageCursors.OfInput[Input] {

  /** Index of the current page. */
  override def pageIndex: Int =
    currentPageCursor.pageIndex

  /** Gives the cursor for the current page. */
  def currentPageCursor: PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize] =
    lens.get(currentPage)

  /** Gives the input for the previous page. */
  def previousPage: Option[Input] = previousPageCursor.map(lens.set(_, currentPage))

  /** Gives the input for the next page. */
  def nextPage: Option[Input] = nextPageCursor.map(lens.set(_, currentPage))
}
object PageCursors {
  trait OfInput[+Input] extends PageCursor.PageIndex {

    /** The input for the current page */
    def currentPage: Input

    /** The input for the previous page */
    def previousPage: Option[Input]

    /** The input for the next page */
    def nextPage: Option[Input]

    /** Windens the input type to a supertype. */
    def widen[Input1 >: Input]: OfInput[Input1] = this
  }
}
