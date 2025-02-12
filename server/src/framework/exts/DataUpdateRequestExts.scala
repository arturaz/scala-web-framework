package framework.exts

import cats.syntax.all.*
import framework.api.{DataUpdateRequest, RequestSuccessful}
import framework.db.*
import difflicious.Differ
import difflicious.DiffResult

implicit class DataUpdateRequestExts[Id, Data](private val request: DataUpdateRequest[Id, Data]) extends AnyVal {

  /** Returns the SQL query that updates the data. */
  def query(
    tableName: TableName,
    idColumn: Column[Id],
    queryLabel: String,
    dataToFragments: Data => NonEmptyVector[(Fragment, Fragment)],
  ): Update0 = {
    val expectedValues =
      dataToFragments(request.expected).map { case (col, value) => fr0"$col = $value" }.intercalate(fr0" AND ")
    val columnsToSet = dataToFragments(request.toSet)
    sql"${tableName.updateTable(columnsToSet)} WHERE ${idColumn === request.id} AND $expectedValues"
      .updateWithLabel(queryLabel)
  }

  /** Returns the [[ConnectionIO]] that performs the update. */
  def connectionIO(
    tableName: TableName,
    idColumn: Column[Id],
    queryLabel: String,
    dataToFragments: Data => NonEmptyVector[(Fragment, Fragment)],
  ): ConnectionIO[RequestSuccessful] = {
    query(tableName, idColumn, queryLabel, dataToFragments).run.map(updatedRows => RequestSuccessful(updatedRows == 1))
  }

  /** Returns an [[OptionT]] that performs the update. Returns `Some` if the update was successful, `None` otherwise. */
  def connectionIOAsOptionT(
    tableName: TableName,
    idColumn: Column[Id],
    queryLabel: String,
    dataToFragments: Data => NonEmptyVector[(Fragment, Fragment)],
  ): OptionT[ConnectionIO, Unit] =
    OptionT(
      query(tableName, idColumn, queryLabel, dataToFragments).run.map(updatedRows => Option.when(updatedRows == 1)(()))
    )

  /** Returns the data difference between the expected and the new set. */
  def dataDiff(using differ: Differ[Data]): DiffResult = {
    differ.diff(expected = request.expected, obtained = request.toSet)
  }
}
