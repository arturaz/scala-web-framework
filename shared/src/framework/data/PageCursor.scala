package framework.data

import framework.prelude.*
import sttp.tapir.DecodeResult

/** A cursor for a paginated list.
  *
  * @tparam PageSize
  *   the type of the page size. This would usually be an `enum` or a refined type preventing malicious clients from
  *   asking for really large pages.
  * @param pageSize
  *   the page size to ask for.
  */
final case class PageCursor[DocId, Timestamp, PageSize](
  cursor: Option[PageCursor.Cursor[DocId, Timestamp]],
  pageSize: PageSize,
)
object PageCursor {
  given circeCodec[DocId, Timestamp, PageSize](using
    CirceEncoder[DocId],
    CirceDecoder[DocId],
    CirceEncoder[Timestamp],
    CirceDecoder[Timestamp],
    CirceEncoder[PageSize],
    CirceDecoder[PageSize],
  ): CirceCodec[PageCursor[DocId, Timestamp, PageSize]] =
    CirceCodec.derived

  given tapirCodec[DocId, Timestamp, PageSize](using
    cursorCodec: TapirCodec[String, Cursor[DocId, Timestamp], TapirCodecFormat.TextPlain],
    pageSizeCodec: TapirCodec[String, PageSize, TapirCodecFormat.TextPlain],
  ): TapirCodec[String, PageCursor[DocId, Timestamp, PageSize], TapirCodecFormat.TextPlain] =
    TapirCodec.string
      .mapDecode { str =>
        def decodePageSize(pageSizeRaw: String) =
          pageSizeCodec.decode(pageSizeRaw.stripPrefix("ps:"))

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
        case PageCursor(None, pageSize) => show"ps:${pageSizeCodec.encode(pageSize)}"
        case PageCursor(Some(cursor), pageSize) =>
          show"ps:${pageSizeCodec.encode(pageSize)},${cursorCodec.encode(cursor)}"
      }

  /** @param id
    *   The last document in the previous page.
    * @param timestamp
    *   The timestamp of the last document in the previous page.
    */
  case class Cursor[DocId, Timestamp](id: DocId, timestamp: Timestamp)
  object Cursor {
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
    ): TapirCodec[String, Cursor[DocId, Timestamp], TapirCodecFormat.TextPlain] =
      TapirCodec.string.mapDecode { str =>
        str.split(",", 2) match {
          case Array(docIdRaw, tsRaw) =>
            for {
              docId <- docIdCodec.decode(docIdRaw.stripPrefix("id:"))
              ts <- timestampCodec.decode(tsRaw.stripPrefix("ts:"))
            } yield apply(docId, ts)
          case other =>
            DecodeResult.Error(str, new Exception(show"Expected 2 parts, got ${other.length} parts"))
        }
      } { cursor =>
        show"id:${docIdCodec.encode(cursor.id)},ts:${timestampCodec.encode(cursor.timestamp)}"
      }
  }
}
