package framework.data

import alleycats.Empty
import framework.prelude.*
import framework.exts.*
import framework.utils.{NamedEnum, UrlConvertible}
import sttp.tapir.DecodeResult
import urldsl.errors.DummyError

/** Helper that implements cursor-based pagination.
  *
  * Cursor-based pagination is more performant and correct than offset-based (for example: `OFFSET 50 LIMIT 10`)
  * pagination, however it only gives you one page at a time.
  *
  * Typically you would define your own cursor type, for example:
  * {{{
  *   type DocumentsPageCursor = PageCursor[FrameworkDateTime, AppDocumentId, AppPageSize]
  * }}}
  *
  * In the server module there are extensions on the `PageCursor` that implement cursor-based pagination, notably
  * `sqlWhereFragment`, `sqlOrderFragment`, `sqlLimitFragment` and `processResults`. You probably also want
  * `PageCursor.hasSurroundingPages` to determine whether the previous/next page are available.
  *
  * @tparam PrimaryColumn
  *   the type of the primary column, ordering is done by primarily by this.
  * @tparam SecondaryColumn
  *   the type of the secondary column, used to distinguish between records that have the same [[PrimaryColumn]].
  * @tparam PageSize
  *   the type of the page size. This would usually be an `enum` or a refined type preventing malicious clients from
  *   asking for really large pages.
  * @param pageSize
  *   the page size to ask for.
  */
final case class PageCursor[+PrimaryColumn, +SecondaryColumn, +PageSize](
  cursor: Option[PageCursor.Cursor[PrimaryColumn, SecondaryColumn]],
  pageSize: PageSize,
) {
  override def toString(): String =
    s"PageCursor(pageSize = ${pageSize}, cursor = ${cursor})"

  /** Index of the current page. */
  def pageIndex: Int =
    cursor.fold(0)(_.index)

  /** Returns a cursor for the first page. */
  def firstPage: PageCursor[PrimaryColumn, SecondaryColumn, PageSize] =
    PageCursor.withPageSize(pageSize).first

  /** Change the page size.
    *
    * @note
    *   Returns to the first page because otherwise the cursor would be invalid and might point to a non-Int or
    *   non-existent page.
    */
  def withPageSize[PS >: PageSize](pageSize: PS): PageCursor[PrimaryColumn, SecondaryColumn, PS] =
    PageCursor.withPageSize(pageSize).first

  /** Returns the [[PageCursorDirection]] of the current cursor. */
  def direction: PageCursorDirection =
    cursor.fold(PageCursorDirection.Forward)(_.direction)

  def next[PrimaryColumn1 >: PrimaryColumn, SecondaryColumn1 >: SecondaryColumn](
    lastPrimary: PrimaryColumn1,
    lastSecondary: SecondaryColumn1,
  ): PageCursor[PrimaryColumn1, SecondaryColumn1, PageSize] =
    PageCursor.withPageSize(pageSize).next(lastPrimary, lastSecondary, pageIndex)

  def next[Data, PrimaryColumn1 >: PrimaryColumn, SecondaryColumn1 >: SecondaryColumn](data: IndexedSeq[Data])(
    getPrimary: Data => PrimaryColumn1,
    getSecondary: Data => SecondaryColumn1,
  ): Option[PageCursor[PrimaryColumn1, SecondaryColumn1, PageSize]] =
    PageCursor.withPageSize(pageSize).next(data, pageIndex)(getPrimary, getSecondary)

  def next[Data, PrimaryColumn1 >: PrimaryColumn, SecondaryColumn1 >: SecondaryColumn](data: NonEmptyVector[Data])(
    getPrimary: Data => PrimaryColumn1,
    getSecondary: Data => SecondaryColumn1,
  ): PageCursor[PrimaryColumn1, SecondaryColumn1, PageSize] =
    PageCursor.withPageSize(pageSize).next(data, pageIndex)(getPrimary, getSecondary)

  def previous[PrimaryColumn1 >: PrimaryColumn, SecondaryColumn1 >: SecondaryColumn](
    firstPrimaryColumn: PrimaryColumn1,
    firstSecondaryColumn: SecondaryColumn1,
  ): PageCursor[PrimaryColumn1, SecondaryColumn1, PageSize] =
    PageCursor.withPageSize(pageSize).previous(firstPrimaryColumn, firstSecondaryColumn, pageIndex)

  def previous[Data, PrimaryColumn1 >: PrimaryColumn, SecondaryColumn1 >: SecondaryColumn](data: IndexedSeq[Data])(
    getPrimary: Data => PrimaryColumn1,
    getSecondary: Data => SecondaryColumn1,
  ): Option[PageCursor[PrimaryColumn1, SecondaryColumn1, PageSize]] =
    PageCursor.withPageSize(pageSize).previous(data, pageIndex)(getPrimary, getSecondary)

  def previous[Data, PrimaryColumn1 >: PrimaryColumn, SecondaryColumn1 >: SecondaryColumn](data: NonEmptyVector[Data])(
    getPrimary: Data => PrimaryColumn1,
    getSecondary: Data => SecondaryColumn1,
  ): PageCursor[PrimaryColumn1, SecondaryColumn1, PageSize] =
    PageCursor.withPageSize(pageSize).previous(data, pageIndex)(getPrimary, getSecondary)
}
object PageCursor {

  /** Creates a builder for cursors that are of page size [[PageSize]]. */
  def withPageSize[PageSize](pageSize: PageSize): Builder[PageSize] = Builder(pageSize)

  case class Builder[PageSize](pageSize: PageSize) extends AnyVal {

    /** Cursor for the first page. */
    def first: PageCursor[Nothing, Nothing, PageSize] = PageCursor(None, pageSize)

    /** Cursor for the next page.
      *
      * @param id
      *   the last document in the current page
      * @param timestamp
      *   the timestamp of the last document in the current page
      * @param index
      *   the index of the current page
      */
    def next[PrimaryColumn, SecondaryColumn](
      primary: PrimaryColumn,
      secondary: SecondaryColumn,
      index: Int,
    ): PageCursor[PrimaryColumn, SecondaryColumn, PageSize] =
      PageCursor(Some(PageCursor.Cursor(PageCursorDirection.Forward, primary, secondary, index + 1)), pageSize)

    /** Cursor for the next page given data for the current page.
      *
      * @param index
      *   the index of the current page
      */
    def next[Data, PrimaryColumn, SecondaryColumn](data: IndexedSeq[Data], index: Int)(
      getPrimary: Data => PrimaryColumn,
      getSecondary: Data => SecondaryColumn,
    ): Option[PageCursor[PrimaryColumn, SecondaryColumn, PageSize]] =
      data.lastOption.map(d => next(getPrimary(d), getSecondary(d), index))

    /** Cursor for the next page given data for the current page.
      *
      * @param index
      *   the index of the current page
      */
    def next[Data, PrimaryColumn, SecondaryColumn](data: NonEmptyVector[Data], index: Int)(
      getPrimary: Data => PrimaryColumn,
      getSecondary: Data => SecondaryColumn,
    ): PageCursor[PrimaryColumn, SecondaryColumn, PageSize] = {
      val last = data.last
      next(getPrimary(last), getSecondary(last), index)
    }

    /** Cursor for the previous page.
      *
      * @param id
      *   the first document in the current page
      * @param timestamp
      *   the timestamp of the first document in the current page
      * @param index
      *   the index of the current page
      */
    def previous[PrimaryColumn, SecondaryColumn](
      primary: PrimaryColumn,
      secondary: SecondaryColumn,
      index: Int,
    ): PageCursor[PrimaryColumn, SecondaryColumn, PageSize] =
      PageCursor(Some(PageCursor.Cursor(PageCursorDirection.Backward, primary, secondary, index - 1)), pageSize)

    /** Cursor for the previous page given data for the current page.
      *
      * @param index
      *   the index of the current page
      */
    def previous[Data, PrimaryColumn, SecondaryColumn](data: IndexedSeq[Data], index: Int)(
      getPrimary: Data => PrimaryColumn,
      getSecondary: Data => SecondaryColumn,
    ): Option[PageCursor[PrimaryColumn, SecondaryColumn, PageSize]] =
      data.headOption.map(d => previous(getPrimary(d), getSecondary(d), index))

    /** Cursor for the previous page given data for the current page.
      *
      * @param index
      *   the index of the current page
      */
    def previous[Data, PrimaryColumn, SecondaryColumn](data: NonEmptyVector[Data], index: Int)(
      getPrimary: Data => PrimaryColumn,
      getSecondary: Data => SecondaryColumn,
    ): PageCursor[PrimaryColumn, SecondaryColumn, PageSize] = {
      val first = data.head
      previous(getPrimary(first), getSecondary(first), index)
    }
  }

  given show[SecondaryColumn: Show, PrimaryColumn: Show, PageSize: Show]
    : Show[PageCursor[PrimaryColumn, SecondaryColumn, PageSize]] =
    c => show"PageCursor(pageSize = ${c.pageSize}, cursor = ${c.cursor})"

  /** Cursor for the first page. */
  given empty[PrimaryColumn, SecondaryColumn, PageSize: Empty]
    : Empty[PageCursor[PrimaryColumn, SecondaryColumn, PageSize]] =
    Empty(apply[PrimaryColumn, SecondaryColumn, PageSize](None, Empty[PageSize].empty))

  given circeCodec[PrimaryColumn, SecondaryColumn, PageSize](using
    CirceEncoder[PrimaryColumn],
    CirceDecoder[PrimaryColumn],
    CirceEncoder[SecondaryColumn],
    CirceDecoder[SecondaryColumn],
    CirceEncoder[PageSize],
    CirceDecoder[PageSize],
  ): CirceCodec[PageCursor[PrimaryColumn, SecondaryColumn, PageSize]] =
    CirceCodec.derived

  /** Default way to encode a cursor so it could be used in Tapir URI paths. */
  given tapirCodec[PrimaryColumn, SecondaryColumn, PageSize](using
    cursorCodec: TapirCodec[String, Cursor[PrimaryColumn, SecondaryColumn], TapirCodecFormat.TextPlain],
    pageSizeCodec: TapirCodec[String, PageSize, TapirCodecFormat.TextPlain],
  ): TapirCodec[String, PageCursor[PrimaryColumn, SecondaryColumn, PageSize], TapirCodecFormat.TextPlain] = {
    val prefix = "ps-"
    TapirCodec.string
      .mapDecode { str =>
        def decodePageSize(pageSizeRaw: String) =
          pageSizeCodec.decode(pageSizeRaw.stripPrefix(prefix))

        str.split(",", 2) match {
          case Array(pageSizeRaw) =>
            for {
              pageSize <- decodePageSize(pageSizeRaw)
            } yield apply(None, pageSize)
          case Array(pageSizeRaw, cursorRaw) =>
            for {
              pageSize <- decodePageSize(pageSizeRaw)
              cursor <- cursorCodec.decode(cursorRaw)
            } yield apply(Some(cursor), pageSize)
          case other =>
            DecodeResult.Error(str, new Exception(show"Expected 2 parts, got ${other.length} parts"))
        }
      } {
        case PageCursor(None, pageSize) => show"$prefix${pageSizeCodec.encode(pageSize)}"
        case PageCursor(Some(cursor), pageSize) =>
          show"$prefix${pageSizeCodec.encode(pageSize)},${cursorCodec.encode(cursor)}"
      }
  }

  /** Default way to encode a cursor so it could be used in Waypoint routes. */
  given urlConvertible[PrimaryColumn, SecondaryColumn, PageSize](using
    TapirCodec[String, PageCursor[PrimaryColumn, SecondaryColumn, PageSize], TapirCodecFormat.TextPlain]
  ): UrlConvertible[PageCursor[PrimaryColumn, SecondaryColumn, PageSize], DummyError] = UrlConvertible.fromCodec

  /** @param primary
    *   The primary column of first/last row in the previous page.
    * @param secondary
    *   The secondary column of the first/last row in the previous page.
    * @param index
    *   The index of the current page. This is not validated in any way and is only used for display purposes.
    */
  case class Cursor[+PrimaryColumn, +SecondaryColumn](
    direction: PageCursorDirection,
    primary: PrimaryColumn,
    secondary: SecondaryColumn,
    index: Int,
  ) {
    override def toString(): String =
      s"Cursor($direction, primary = $primary, secondary = $secondary, current page index = $index)"
  }
  object Cursor {
    given show[PrimaryColumn: Show, SecondaryColumn: Show]: Show[Cursor[PrimaryColumn, SecondaryColumn]] =
      c =>
        show"Cursor(${c.direction}, primary = ${c.primary}, secondary = ${c.secondary}, current page index = ${c.index})"

    given circeCodec[PrimaryColumn, SecondaryColumn](using
      CirceEncoder[PrimaryColumn],
      CirceDecoder[PrimaryColumn],
      CirceEncoder[SecondaryColumn],
      CirceDecoder[SecondaryColumn],
    ): CirceCodec[Cursor[PrimaryColumn, SecondaryColumn]] =
      CirceCodec.derived

    given tapirCodec[PrimaryColumn, SecondaryColumn](using
      primaryCodec: TapirCodec[String, PrimaryColumn, TapirCodecFormat.TextPlain],
      secondaryCodec: TapirCodec[String, SecondaryColumn, TapirCodecFormat.TextPlain],
    ): TapirCodec[String, Cursor[PrimaryColumn, SecondaryColumn], TapirCodecFormat.TextPlain] = {
      val primaryPrefix = "p-"
      val secondaryPrefix = "s-"
      val idxPrefix = "idx-"
      val idxCodec = TapirCodec.int

      TapirCodec.string.mapDecode { str =>
        val parts = 4
        str.split(",", parts) match {
          case Array(directionRaw, docIdRaw, tsRaw, idxRaw) =>
            for {
              direction <- PageCursorDirection.tapirCodec.decode(directionRaw)
              primary <- primaryCodec.decode(tsRaw.stripPrefix(primaryPrefix))
              secondary <- secondaryCodec.decode(docIdRaw.stripPrefix(secondaryPrefix))
              idx <- idxCodec.decode(idxRaw.stripPrefix(idxPrefix))
            } yield apply(direction, primary, secondary, idx)
          case other =>
            DecodeResult.Error(str, new Exception(show"Expected $parts parts, got ${other.length} parts"))
        }
      } { cursor =>
        show"${PageCursorDirection.tapirCodec.encode(cursor.direction)}" +
          show"$primaryPrefix${primaryCodec.encode(cursor.primary)}," +
          show"$secondaryPrefix${secondaryCodec.encode(cursor.secondary)},$idxPrefix${idxCodec.encode(cursor.index)}"
      }
    }

    given urlConvertible[PrimaryColumn, SecondaryColumn](using
      TapirCodec[String, Cursor[PrimaryColumn, SecondaryColumn], TapirCodecFormat.TextPlain]
    ): UrlConvertible[Cursor[PrimaryColumn, SecondaryColumn], DummyError] =
      UrlConvertible.fromCodec
  }
}
