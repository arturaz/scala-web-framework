package framework.exts

import doobie.AliasedTableDefinition
import framework.data.{HasSurroundingPages, PageCursor, PageCursorDirection}
import framework.db.*

extension [DocId, Timestamp, PageSize](cursor: PageCursor[DocId, Timestamp, PageSize]) {

  /** Produces the SQL fragment that filters the previous/next page. */
  def sqlWhereFragment(colId: Column[DocId], colTimestamp: Column[Timestamp], order: SqlOrder)(using
    Write[DocId],
    Write[Timestamp],
  ): Fragment = cursor.cursor match
    case None         => sql"1=1"
    case Some(cursor) => cursor.sqlWhereFragment(colId, colTimestamp, order)

  /* Produces the SQL fragment for `ORDER BY` clause that orders by timestamp and then id if the timestamps match. */
  def sqlOrderFragment(colId: Column[DocId], colTimestamp: Column[Timestamp], order: SqlOrder): Fragment = {
    val effectiveOrder = cursor.direction match {
      case PageCursorDirection.Backward =>
        /** When going backwards with a cursor we must invert the order because otherwise we always get the first page
          * instead of the previous one. After we fetch results we must reverse the order again using
          * [[processResults]].
          */
        order.reverse
      case PageCursorDirection.Forward => order
    }
    sql"$colTimestamp $effectiveOrder, $colId $effectiveOrder"
  }

  /** Invoke this after fetching results from the database.
    *
    * This is needed for [[PageCursorDirection.Backward]] to correctly render the fetched results.
    */
  def processResults[C[X] <: Seq[X], A](data: C[A])(using factory: collection.Factory[A, C[A]]): C[A] = {
    cursor.direction match
      case PageCursorDirection.Backward => data.reverseIterator.to(factory)
      case PageCursorDirection.Forward  => data
  }

  /** Produces the SQL fragment that limits the number of documents in the current page. */
  def sqlLimitFragment(using Write[PageSize]): Fragment =
    sql"LIMIT ${cursor.pageSize}"
}

extension (c: PageCursor.type) {

  /** Checks with the database whether the previous/next page are available when using cursor-based pagination.
    *
    * @param table
    *   the table to check
    * @param whereFragment
    *   the `WHERE` clause fragment that was used to query the current page
    * @param data
    *   the data of the current page
    */
  def hasSurroundingPages[Table <: TableDefinition, Data[X] <: IndexedSeq[X], A, DocId, Timestamp](
    table: AliasedTableDefinition[Table],
    whereFragment: Option[AliasedTableDefinition[Table] => Fragment],
    data: Data[A],
    idCol: AliasedTableDefinition[Table] => Column[DocId],
    getId: A => DocId,
    timestampCol: AliasedTableDefinition[Table] => Column[Timestamp],
    getTimestamp: A => Timestamp,
    order: SqlOrder,
  )(using
    Write[DocId],
    Write[Timestamp],
  ): ConnectionIO[HasSurroundingPages[Data[A]]] = {
    if (data.isEmpty) ConnectionIO.pure(HasSurroundingPages.withoutPages(data))
    else {
      val first = data.head
      val firstId = getId(first)
      val firstTimestamp = getTimestamp(first)
      val last = data.last
      val lastId = getId(last)
      val lastTimestamp = getTimestamp(last)

      val whereSql = whereFragment.fold(sql"1=1")(fn => fn(table))
      val colId = idCol(table)
      val colTimestamp = timestampCol(table)

      val (prevCondition, nextCondition) = order match
        case SqlOrder.Asc =>
          (
            sql"$colTimestamp <= $firstTimestamp AND ($colTimestamp < $firstTimestamp OR $colId < $firstId)",
            sql"$colTimestamp >= $lastTimestamp AND ($colTimestamp > $lastTimestamp OR $colId > $lastId)",
          )
        case SqlOrder.Desc =>
          (
            sql"$colTimestamp >= $firstTimestamp AND ($colTimestamp > $firstTimestamp OR $colId > $firstId)",
            sql"$colTimestamp <= $lastTimestamp AND ($colTimestamp < $lastTimestamp OR $colId < $lastId)",
          )

      val q = sql"""
        SELECT 
          (EXISTS (SELECT 1 FROM $table 
            WHERE 
              ($whereSql)
              AND $prevCondition
            ORDER BY $colTimestamp $order, $colId $order
            LIMIT 1
          )) as has_previous,
          (EXISTS (SELECT 1 FROM $table 
            WHERE 
              ($whereSql)
              AND $nextCondition
            ORDER BY $colTimestamp $order, $colId $order
            LIMIT 1
          )) as has_next
      """.query[(Boolean, Boolean)]

      q.unique.map { case (hasPrevious, hasNext) =>
        HasSurroundingPages(
          data,
          HasSurroundingPages.Pages(hasPrevious = hasPrevious, hasNext = hasNext),
        )
      }
    }
  }
}

extension [DocId, Timestamp](cursor: PageCursor.Cursor[DocId, Timestamp]) {

  /** Produces the SQL fragment that filters the previous/next page. */
  def sqlWhereFragment(colId: Column[DocId], colTimestamp: Column[Timestamp], order: SqlOrder)(using
    Write[DocId],
    Write[Timestamp],
  ): Fragment =
    (cursor.direction, order) match
      case (PageCursorDirection.Backward, SqlOrder.Asc) | (PageCursorDirection.Forward, SqlOrder.Desc) =>
        sql"($colTimestamp <= ${cursor.timestamp} AND ($colTimestamp < ${cursor.timestamp} OR $colId < ${cursor.id}))"
      case (PageCursorDirection.Forward, SqlOrder.Asc) | (PageCursorDirection.Backward, SqlOrder.Desc) =>
        sql"($colTimestamp >= ${cursor.timestamp} AND ($colTimestamp > ${cursor.timestamp} OR $colId > ${cursor.id}))"
}
