package framework.data

import framework.prelude.*

/** Indicates whether the previous/next page are available. */
case class HasSurroundingPages[Data](data: Data, pages: HasSurroundingPages.Pages) derives CanEqual {

  /** Returns the cursor for the previous page if that is available. */
  def previousPageCursor[PrimaryColumn, SecondaryColumn, PageSize](
    currentPageCursor: PageCursor[PrimaryColumn, SecondaryColumn, PageSize],
    getFirst: Data => Option[(PrimaryColumn, SecondaryColumn)],
  ): Option[PageCursor[PrimaryColumn, SecondaryColumn, PageSize]] = {
    if (!pages.hasPrevious) None
    // If we are on Page 2 then return the cursor for the first page instead of returning a cursor that goes backwards
    // from page 2.
    else if (currentPageCursor.pageIndex == 1) Some(currentPageCursor.firstPage)
    else getFirst(data).map(currentPageCursor.previous.tupled)
  }

  /** Returns the cursor for the next page if that is available. */
  def nextPageCursor[PrimaryColumn, SecondaryColumn, PageSize](
    currentPageCursor: PageCursor[PrimaryColumn, SecondaryColumn, PageSize],
    getLast: Data => Option[(PrimaryColumn, SecondaryColumn)],
  ): Option[PageCursor[PrimaryColumn, SecondaryColumn, PageSize]] = {
    if (!pages.hasNext) None
    else
      getLast(data).map { case (id, timestamp) =>
        currentPageCursor.next(id, timestamp)
      }
  }
}
object HasSurroundingPages {
  case class Pages(hasPrevious: Boolean, hasNext: Boolean) derives CanEqual, CirceCodec {
    override def toString(): String = show"Pages(prev = $hasPrevious, next = $hasNext)"
  }

  def withoutPages[Data](data: Data): HasSurroundingPages[Data] =
    apply(data, Pages(hasPrevious = false, hasNext = false))

  def apply[Data](data: Data, hasPrevious: Boolean, hasNext: Boolean): HasSurroundingPages[Data] =
    apply(data, Pages(hasPrevious = hasPrevious, hasNext = hasNext))

  given circeCodec[Data: CirceEncoder: CirceDecoder]: CirceCodec[HasSurroundingPages[Data]] = CirceCodec.derived
}
