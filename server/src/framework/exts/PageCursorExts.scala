package framework.exts

import doobie.AliasedTableDefinition
import doobie.util.compat.FactoryCompat
import framework.data.{HasSurroundingPages, PageCursor, PageCursorDirection}
import framework.db.*
import io.scalaland.chimney.Transformer

extension [PrimaryColumn, SecondaryColumn, PageSize](cursor: PageCursor[PrimaryColumn, SecondaryColumn, PageSize]) {

  /** Produces the SQL fragment that filters the previous/next page. */
  def sqlWhereFragment(colPrimary: Column[PrimaryColumn], colSecondary: Column[SecondaryColumn], order: SqlOrder)(using
    Write[PrimaryColumn],
    Write[SecondaryColumn],
  ): Fragment = cursor.cursor match
    case None         => sql"1=1"
    case Some(cursor) => cursor.sqlWhereFragment(colPrimary, colSecondary, order)

  /* Produces the SQL fragment for `ORDER BY` clause that orders by timestamp and then id if the timestamps match. */
  def sqlOrderFragment(
    colPrimary: Column[PrimaryColumn],
    colSecondary: Column[SecondaryColumn],
    order: SqlOrder,
  ): Fragment = {
    val effectiveOrder = cursor.direction match {
      case PageCursorDirection.Backward =>
        /** When going backwards with a cursor we must invert the order because otherwise we always get the first page
          * instead of the previous one. After we fetch results we must reverse the order again using
          * [[processResults]].
          */
        order.reverse
      case PageCursorDirection.Forward => order
    }
    sql"$colPrimary $effectiveOrder, $colSecondary $effectiveOrder"
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
  def sqlLimitFragment(using t: Transformer[PageSize, Long]): Fragment =
    sql"LIMIT ${t.transform(cursor.pageSize)}"

  /** Constructs the SQL query that fetches the appropriate page. */
  def sqlQuery[Result](
    tables: Fragment,
    columns: Columns[Result],
    whereFragment: Option[Fragment],
    colPrimary: Column[PrimaryColumn],
    colSecondary: Column[SecondaryColumn],
    order: SqlOrder,
  )(using Write[PrimaryColumn], Write[SecondaryColumn], Read[Result], Transformer[PageSize, Long]): Query0[Result] = {
    val whereSql = whereFragment.getOrElse(sql"1=1")

    sql"""
      SELECT $columns FROM $tables
      WHERE 
        $whereSql
        AND ${cursor.sqlWhereFragment(colPrimary, colSecondary, order)}
      ORDER BY ${cursor.sqlOrderFragment(colPrimary, colSecondary, order)}
      ${cursor.sqlLimitFragment}
    """.queryOf(columns)
  }

  /** Returns the [[ConnectionIO]] that fetches results and checks whether the previous/next page are available.
    *
    * Additionally transforms the fetched rows using `process`.
    */
  def connectionIOMapped[RowsResult, ProcessedResult](
    tables: Fragment,
    columns: Columns[RowsResult],
    whereFragment: Option[Fragment],
    colPrimary: Column[PrimaryColumn],
    colSecondary: Column[SecondaryColumn],
    order: SqlOrder,
    process: RowsResult => ProcessedResult,
    getPrimary: ProcessedResult => PrimaryColumn,
    getSecondary: ProcessedResult => SecondaryColumn,
  )(using
    Write[PrimaryColumn],
    Write[SecondaryColumn],
    Read[RowsResult],
    Transformer[PageSize, Long],
  ): PageCursorConnectionIOBuilder[PrimaryColumn, SecondaryColumn, PageSize, RowsResult, ProcessedResult] = {
    PageCursorConnectionIOBuilder[PrimaryColumn, SecondaryColumn, PageSize, RowsResult, ProcessedResult](
      cursor = cursor,
      tables = tables,
      columns = columns,
      process = process,
      whereFragment = whereFragment,
      colSecondary = colSecondary,
      getSecondary = getSecondary,
      colPrimary = colPrimary,
      getPrimary = getPrimary,
      order = order,
    )
  }

  /** Returns the [[ConnectionIO]] that fetches results and checks whether the previous/next page are available. */
  def connectionIO[Result](
    tables: Fragment,
    columns: Columns[Result],
    whereFragment: Option[Fragment],
    colPrimary: Column[PrimaryColumn],
    colSecondary: Column[SecondaryColumn],
    order: SqlOrder,
    getPrimary: Result => PrimaryColumn,
    getSecondary: Result => SecondaryColumn,
  )(using
    Write[PrimaryColumn],
    Write[SecondaryColumn],
    Read[Result],
    Transformer[PageSize, Long],
  ): PageCursorConnectionIOBuilder[PrimaryColumn, SecondaryColumn, PageSize, Result, Result] = {
    cursor.connectionIOMapped(
      tables = tables,
      columns = columns,
      whereFragment = whereFragment,
      order = order,
      colSecondary = colSecondary,
      colPrimary = colPrimary,
      process = identity,
      getSecondary = getSecondary,
      getPrimary = getPrimary,
    )
  }
}

case class PageCursorConnectionIOBuilder[PrimaryColumn, SecondaryColumn, PageSize, RowsResult, ProcessedResult](
  cursor: PageCursor[PrimaryColumn, SecondaryColumn, PageSize],
  tables: Fragment,
  columns: Columns[RowsResult],
  process: RowsResult => ProcessedResult,
  whereFragment: Option[Fragment],
  colPrimary: Column[PrimaryColumn],
  getPrimary: ProcessedResult => PrimaryColumn,
  colSecondary: Column[SecondaryColumn],
  getSecondary: ProcessedResult => SecondaryColumn,
  order: SqlOrder,
)(using
  Write[PrimaryColumn],
  Write[SecondaryColumn],
  Read[RowsResult],
  Transformer[PageSize, Long],
) {
  def queryWithoutProcessing: Query0[RowsResult] =
    cursor.sqlQuery(tables, columns, whereFragment, colPrimary, colSecondary, order)

  def query: Query0[ProcessedResult] =
    queryWithoutProcessing.map(process)

  def to[C[X] <: IndexedSeq[X]](using
    collection.Factory[ProcessedResult, C[ProcessedResult]]
  ): ConnectionIO[HasSurroundingPages[C[ProcessedResult]]] =
    for {
      rows <- query.to[C].map(cursor.processResults)
      result <- PageCursor.hasSurroundingPages(
        tables = tables,
        whereFragment = whereFragment,
        data = rows,
        secondaryCol = colSecondary,
        getSecondary = getSecondary,
        primaryCol = colPrimary,
        getPrimary = getPrimary,
        order = order,
      )
    } yield result
}

extension (c: PageCursor.type) {

  /** Checks with the database whether the previous/next page are available when using cursor-based pagination.
    *
    * @param tables
    *   the tables to check. This is put after the `FROM` clause and can be not only the table name, but joins as well.
    * @param whereFragment
    *   the `WHERE` clause fragment that was used to query the current page. Can be [[None]] if no additional filtering
    *   is needed.
    * @param data
    *   the data of the current page
    */
  def hasSurroundingPages[Data[X] <: IndexedSeq[X], A, PrimaryColumn, SecondaryColumn](
    tables: Fragment,
    whereFragment: Option[Fragment],
    data: Data[A],
    primaryCol: Column[PrimaryColumn],
    getPrimary: A => PrimaryColumn,
    secondaryCol: Column[SecondaryColumn],
    getSecondary: A => SecondaryColumn,
    order: SqlOrder,
  )(using
    Write[SecondaryColumn],
    Write[PrimaryColumn],
  ): ConnectionIO[HasSurroundingPages[Data[A]]] = {
    if (data.isEmpty) ConnectionIO.pure(HasSurroundingPages.withoutPages(data))
    else {
      val first = data.head
      val firstPrimary = getPrimary(first)
      val firstSecondary = getSecondary(first)
      val last = data.last
      val lastPrimary = getPrimary(last)
      val lastSecondary = getSecondary(last)

      val whereSql = whereFragment.getOrElse(sql"1=1")

      val (prevCondition, nextCondition) = order match
        case SqlOrder.Asc =>
          (
            sql"$primaryCol <= $firstPrimary AND ($primaryCol < $firstPrimary OR $secondaryCol < $firstSecondary)",
            sql"$primaryCol >= $lastPrimary AND ($primaryCol > $lastPrimary OR $secondaryCol > $lastSecondary)",
          )
        case SqlOrder.Desc =>
          (
            sql"$primaryCol >= $firstPrimary AND ($primaryCol > $firstPrimary OR $secondaryCol > $firstSecondary)",
            sql"$primaryCol <= $lastPrimary AND ($primaryCol < $lastPrimary OR $secondaryCol < $lastSecondary)",
          )

      val q = sql"""
        SELECT 
          (EXISTS (SELECT 1 FROM $tables
            WHERE 
              ($whereSql)
              AND $prevCondition
            ORDER BY $primaryCol $order, $secondaryCol $order
            LIMIT 1
          )) as has_previous,
          (EXISTS (SELECT 1 FROM $tables 
            WHERE 
              ($whereSql)
              AND $nextCondition
            ORDER BY $primaryCol $order, $secondaryCol $order
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

extension [PrimaryColumn, SecondaryColumn](cursor: PageCursor.Cursor[PrimaryColumn, SecondaryColumn]) {

  /** Produces the SQL fragment that filters the previous/next page. */
  def sqlWhereFragment(colPrimary: Column[PrimaryColumn], colSecondary: Column[SecondaryColumn], order: SqlOrder)(using
    Write[PrimaryColumn],
    Write[SecondaryColumn],
  ): Fragment =
    (cursor.direction, order) match
      case (PageCursorDirection.Backward, SqlOrder.Asc) | (PageCursorDirection.Forward, SqlOrder.Desc) =>
        sql"($colPrimary < ${cursor.primary} OR ($colPrimary = ${cursor.primary} AND $colSecondary < ${cursor.secondary}))"
      case (PageCursorDirection.Forward, SqlOrder.Asc) | (PageCursorDirection.Backward, SqlOrder.Desc) =>
        sql"($colPrimary > ${cursor.primary} OR ($colPrimary = ${cursor.primary} AND $colSecondary > ${cursor.secondary}))"
}
