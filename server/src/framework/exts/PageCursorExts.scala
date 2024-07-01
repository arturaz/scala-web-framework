package framework.exts

import framework.data.PageCursor
import framework.db.*

extension [DocId, Timestamp, PageSize](cursor: PageCursor[DocId, Timestamp, PageSize]) {

  /** Produces the SQL fragment that filters the next page. */
  def sqlWhereFrament(colId: Column[DocId], colTimestamp: Column[Timestamp])(using
    Write[DocId],
    Write[Timestamp],
  ): Fragment = cursor.cursor match
    case None         => sql"(1=1)"
    case Some(cursor) => cursor.sqlWhereFragment(colId, colTimestamp)

}

extension [DocId, Timestamp](cursor: PageCursor.Cursor[DocId, Timestamp]) {

  /** Produces the SQL fragment that filters the next page. */
  def sqlWhereFragment(colId: Column[DocId], colTimestamp: Column[Timestamp])(using
    Write[DocId],
    Write[Timestamp],
  ): Fragment =
    sql"($colId > ${cursor.id} AND $colTimestamp > ${cursor.timestamp})"
}
