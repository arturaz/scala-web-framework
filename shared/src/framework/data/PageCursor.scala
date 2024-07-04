package framework.data

import alleycats.Empty
import framework.prelude.*
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
  *   type DocumentsPageCursor = PageCursor[AppDocumentId, AppDateTime, AppPageSize]
  * }}}
  *
  * In the server module there are extensions on the `PageCursor` that implement cursor-based pagination, notably
  * `sqlWhereFragment`, `sqlOrderFragment` and `sqlLimitFragment`. You probably also want
  * `PageCursor.hasSurroundingPages` to determine whether the previous/next page are available.
  *
  * @tparam PageSize
  *   the type of the page size. This would usually be an `enum` or a refined type preventing malicious clients from
  *   asking for really large pages.
  * @param pageSize
  *   the page size to ask for.
  */
final case class PageCursor[+DocId, +Timestamp, +PageSize](
  cursor: Option[PageCursor.Cursor[DocId, Timestamp]],
  pageSize: PageSize,
) {

  /** Index of the current page. */
  def pageIndex: Int =
    cursor.fold(0)(_.index)

  /** Returns the [[PageCursorDirection]] of the current cursor. */
  def direction: PageCursorDirection =
    cursor.fold(PageCursorDirection.Forward)(_.direction)

  def next[DocId1 >: DocId, Timestamp1 >: Timestamp](
    lastId: DocId1,
    lastTimestamp: Timestamp1,
  ): PageCursor[DocId1, Timestamp1, PageSize] =
    PageCursor.withPageSize(pageSize).next(lastId, lastTimestamp, pageIndex)

  def next[Data, DocId1 >: DocId, Timestamp1 >: Timestamp](data: IndexedSeq[Data])(
    getId: Data => DocId1,
    getTimestamp: Data => Timestamp1,
  ): Option[PageCursor[DocId1, Timestamp1, PageSize]] =
    PageCursor.withPageSize(pageSize).next(data, pageIndex)(getId, getTimestamp)

  def next[Data, DocId1 >: DocId, Timestamp1 >: Timestamp](data: NonEmptyVector[Data])(
    getId: Data => DocId1,
    getTimestamp: Data => Timestamp1,
  ): PageCursor[DocId1, Timestamp1, PageSize] =
    PageCursor.withPageSize(pageSize).next(data, pageIndex)(getId, getTimestamp)

  def previous[DocId1 >: DocId, Timestamp1 >: Timestamp](
    firstId: DocId1,
    firstTimestamp: Timestamp1,
  ): PageCursor[DocId1, Timestamp1, PageSize] =
    PageCursor.withPageSize(pageSize).previous(firstId, firstTimestamp, pageIndex)

  def previous[Data, DocId1 >: DocId, Timestamp1 >: Timestamp](data: IndexedSeq[Data])(
    getId: Data => DocId1,
    getTimestamp: Data => Timestamp1,
  ): Option[PageCursor[DocId1, Timestamp1, PageSize]] =
    PageCursor.withPageSize(pageSize).previous(data, pageIndex)(getId, getTimestamp)

  def previous[Data, DocId1 >: DocId, Timestamp1 >: Timestamp](data: NonEmptyVector[Data])(
    getId: Data => DocId1,
    getTimestamp: Data => Timestamp1,
  ): PageCursor[DocId1, Timestamp1, PageSize] =
    PageCursor.withPageSize(pageSize).previous(data, pageIndex)(getId, getTimestamp)
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
    def next[DocId, Timestamp](id: DocId, timestamp: Timestamp, index: Int): PageCursor[DocId, Timestamp, PageSize] =
      PageCursor(Some(PageCursor.Cursor(PageCursorDirection.Forward, id, timestamp, index + 1)), pageSize)

    /** Cursor for the next page given data for the current page.
      *
      * @param index
      *   the index of the current page
      */
    def next[Data, DocId, Timestamp](data: IndexedSeq[Data], index: Int)(
      getId: Data => DocId,
      getTimestamp: Data => Timestamp,
    ): Option[PageCursor[DocId, Timestamp, PageSize]] =
      data.lastOption.map(d => next(getId(d), getTimestamp(d), index))

    /** Cursor for the next page given data for the current page.
      *
      * @param index
      *   the index of the current page
      */
    def next[Data, DocId, Timestamp](data: NonEmptyVector[Data], index: Int)(
      getId: Data => DocId,
      getTimestamp: Data => Timestamp,
    ): PageCursor[DocId, Timestamp, PageSize] = {
      val last = data.last
      next(getId(last), getTimestamp(last), index)
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
    def previous[DocId, Timestamp](
      id: DocId,
      timestamp: Timestamp,
      index: Int,
    ): PageCursor[DocId, Timestamp, PageSize] =
      PageCursor(Some(PageCursor.Cursor(PageCursorDirection.Backward, id, timestamp, index - 1)), pageSize)

    /** Cursor for the previous page given data for the current page.
      *
      * @param index
      *   the index of the current page
      */
    def previous[Data, DocId, Timestamp](data: IndexedSeq[Data], index: Int)(
      getId: Data => DocId,
      getTimestamp: Data => Timestamp,
    ): Option[PageCursor[DocId, Timestamp, PageSize]] =
      data.headOption.map(d => previous(getId(d), getTimestamp(d), index))

    /** Cursor for the previous page given data for the current page.
      *
      * @param index
      *   the index of the current page
      */
    def previous[Data, DocId, Timestamp](data: NonEmptyVector[Data], index: Int)(
      getId: Data => DocId,
      getTimestamp: Data => Timestamp,
    ): PageCursor[DocId, Timestamp, PageSize] = {
      val first = data.head
      previous(getId(first), getTimestamp(first), index)
    }
  }

  given show[DocId: Show, Timestamp: Show, PageSize: Show]: Show[PageCursor[DocId, Timestamp, PageSize]] =
    c => show"PageCursor(pageSize = ${c.pageSize}, cursor = ${c.cursor})"

  /** Cursor for the first page. */
  given empty[DocId, Timestamp, PageSize: Empty]: Empty[PageCursor[DocId, Timestamp, PageSize]] =
    Empty(apply[DocId, Timestamp, PageSize](None, Empty[PageSize].empty))

  given circeCodec[DocId, Timestamp, PageSize](using
    CirceEncoder[DocId],
    CirceDecoder[DocId],
    CirceEncoder[Timestamp],
    CirceDecoder[Timestamp],
    CirceEncoder[PageSize],
    CirceDecoder[PageSize],
  ): CirceCodec[PageCursor[DocId, Timestamp, PageSize]] =
    CirceCodec.derived

  /** Default way to encode a cursor so it could be used in Tapir URI paths. */
  given tapirCodec[DocId, Timestamp, PageSize](using
    cursorCodec: TapirCodec[String, Cursor[DocId, Timestamp], TapirCodecFormat.TextPlain],
    pageSizeCodec: TapirCodec[String, PageSize, TapirCodecFormat.TextPlain],
  ): TapirCodec[String, PageCursor[DocId, Timestamp, PageSize], TapirCodecFormat.TextPlain] = {
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
  given urlConvertible[DocId, Timestamp, PageSize](using
    TapirCodec[String, PageCursor[DocId, Timestamp, PageSize], TapirCodecFormat.TextPlain]
  ): UrlConvertible[PageCursor[DocId, Timestamp, PageSize], DummyError] = UrlConvertible.fromCodec

  /** @param id
    *   The first/last document in the previous page.
    * @param timestamp
    *   The timestamp of the first/last document in the previous page.
    * @param index
    *   The index of the current page. This is not validated in any way and is only used for display purposes.
    */
  case class Cursor[+DocId, +Timestamp](
    direction: PageCursorDirection,
    id: DocId,
    timestamp: Timestamp,
    index: Int,
  )
  object Cursor {
    given show[DocId: Show, Timestamp: Show]: Show[Cursor[DocId, Timestamp]] =
      c => show"Cursor(${c.direction}, id = ${c.id}, timestamp = ${c.timestamp}, index = ${c.index})"

    given circeCodec[DocId, Timestamp](using
      CirceEncoder[DocId],
      CirceDecoder[DocId],
      CirceEncoder[Timestamp],
      CirceDecoder[Timestamp],
    ): CirceCodec[Cursor[DocId, Timestamp]] =
      CirceCodec.derived

    given tapirCodec[DocId, Timestamp](using
      docIdCodec: TapirCodec[String, DocId, TapirCodecFormat.TextPlain],
      timestampCodec: TapirCodec[String, Timestamp, TapirCodecFormat.TextPlain],
    ): TapirCodec[String, Cursor[DocId, Timestamp], TapirCodecFormat.TextPlain] = {
      val idPrefix = "id-"
      val tsPrefix = "ts-"
      val idxPrefix = "idx-"
      val idxCodec = TapirCodec.int

      TapirCodec.string.mapDecode { str =>
        val parts = 4
        str.split(",", parts) match {
          case Array(directionRaw, docIdRaw, tsRaw, idxRaw) =>
            for {
              direction <- PageCursorDirection.tapirCodec.decode(directionRaw)
              docId <- docIdCodec.decode(docIdRaw.stripPrefix(idPrefix))
              ts <- timestampCodec.decode(tsRaw.stripPrefix(tsPrefix))
              idx <- idxCodec.decode(idxRaw.stripPrefix(idxPrefix))
            } yield apply(direction, docId, ts, idx)
          case other =>
            DecodeResult.Error(str, new Exception(show"Expected $parts parts, got ${other.length} parts"))
        }
      } { cursor =>
        show"${PageCursorDirection.tapirCodec.encode(cursor.direction)},$idPrefix${docIdCodec.encode(cursor.id)}," +
          show"$tsPrefix${timestampCodec.encode(cursor.timestamp)},$idxPrefix${idxCodec.encode(cursor.index)}"
      }
    }

    given urlConvertible[DocId, Timestamp](using
      TapirCodec[String, Cursor[DocId, Timestamp], TapirCodecFormat.TextPlain]
    ): UrlConvertible[Cursor[DocId, Timestamp], DummyError] =
      UrlConvertible.fromCodec
  }
}

enum PageCursorDirection derives CanEqual {

  /** Go to the previous page. */
  case Backward

  /** Go to the next page. */
  case Forward
}
object PageCursorDirection {
  given namedEnum: NamedEnum[PageCursorDirection] with {
    override lazy val values: NonEmptyVector[PageCursorDirection] =
      NonEmptyVector.fromVectorUnsafe(PageCursorDirection.values.toVector)

    override def toName(a: PageCursorDirection): String = a match
      case PageCursorDirection.Backward => "bwd"
      case PageCursorDirection.Forward  => "fwd"
  }

  given circeKeyCodec: CirceKeyCodec[PageCursorDirection] = namedEnum.circeKeyCodec
  given circeCode: CirceCodec[PageCursorDirection] = namedEnum.circeCodec
  given schema: Schema[PageCursorDirection] = namedEnum.schema
  given tapirCodec: TapirCodec[String, PageCursorDirection, TapirCodecFormat.TextPlain] = namedEnum.tapirCodec
  given show: Show[PageCursorDirection] = namedEnum.show
}

/** Indicates whether the previous/next page are available. */
case class HasSurroundingPages[Data](data: Data, pages: HasSurroundingPages.Pages) derives CanEqual
object HasSurroundingPages {
  case class Pages(hasPrevious: Boolean, hasNext: Boolean) derives CanEqual, CirceCodec

  def withoutPages[Data](data: Data): HasSurroundingPages[Data] =
    apply(data, Pages(hasPrevious = false, hasNext = false))

  def apply[Data](data: Data, hasPrevious: Boolean, hasNext: Boolean): HasSurroundingPages[Data] =
    apply(data, Pages(hasPrevious = hasPrevious, hasNext = hasNext))

  given circeCodec[Data: CirceEncoder: CirceDecoder]: CirceCodec[HasSurroundingPages[Data]] = CirceCodec.derived
}
