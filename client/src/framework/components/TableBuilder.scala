package framework.components

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.{HTMLTableCellElement, HTMLTableElement, HTMLTableRowElement, HTMLTableSectionElement}
import framework.data.MaybeSignal

/** If you are annoyed by the design of HTML tables and that headers are separated from the body, this is the component
  * for you.
  */
object TableBuilder {
  type Table = ReactiveHtmlElement[HTMLTableElement]
  type TableSection = ReactiveHtmlElement[HTMLTableSectionElement]
  type TableRow = ReactiveHtmlElement[HTMLTableRowElement]
  type TableCell = ReactiveHtmlElement[HTMLTableCellElement]

  case class Column[-Data](
    header: Modifier[TableSection] | Seq[Modifier[TableSection]],
    cell: Signal[Data] => (Modifier[TableRow] | Seq[Modifier[TableRow]]),
  ) {
    def headerSeq: Seq[Modifier[TableSection]] = header.deunionizeSeq

    def cellSeq(data: Signal[Data]): Seq[Modifier[TableRow]] = cell(data).deunionizeSeq
  }

  def apply(tableModifiers: Modifier[Table]*) =
    BuilderStep1(tableModifiers.toSeq)

  case class BuilderStep1(tableModifiers: Seq[Modifier[Table]]) extends AnyVal {
    def of[Data, Key](
      datas: MaybeSignal[Seq[Data]],
      splitBy: Data => Key,
      columns: Seq[Column[Data]],
    ): Builder[Data, Key] = Builder(
      datas.deunionizeSignal,
      dataSplitter = (datas, project) => datas.split(splitBy)(project),
      columns,
      tableModifiers,
    )

    def ofSplitByIndex[Data](
      datas: MaybeSignal[Seq[Data]],
      columns: Seq[Column[Data]],
    ): Builder[Data, Int] = Builder(
      datas.deunionizeSignal,
      dataSplitter = (datas, project) => datas.splitByIndex(project),
      columns,
      tableModifiers,
    )
  }

  /** @param datas
    *   the datum for the rows of the table
    * @param dataSplitter
    *   if [[None]] we will split the data by index.
    * @param columns
    *   the columns of the table
    * @param tableModifiers
    *   the modifiers to apply to the table
    * @param onNoEntriesFound
    *   rendered when [[datas]] are empty.
    * @param rowAmender
    *   allows you to modify each of the rows in the table.
    */
  case class Builder[Data, Key](
    datas: Signal[Seq[Data]],
    dataSplitter: (Signal[Seq[Data]], (Key, Data, Signal[Data]) => TableRow) => Signal[Seq[TableRow]],
    columns: Seq[Column[Data]],
    tableModifiers: Seq[Modifier[Table]],
    onNoEntriesFound: Seq[Modifier[TableCell]] = Seq(cls := "text-center", "No entries found"),
    rowAmender: ((Key, Data, Signal[Data]) => Seq[Modifier[TableRow]]) =
      (_: Key, _: Data, _: Signal[Data]) => Seq.empty,
    header: Seq[TableRow] = Seq.empty,
    footer: Seq[TableRow] = Seq.empty,
  ) {

    /** Sets the [[rowAmender]]. */
    def amendEachRow(amender: (Key, Data, Signal[Data]) => Seq[Modifier[TableRow]]): Builder[Data, Key] =
      copy(rowAmender = amender)

    /** Sets the [[rowAmender]]. */
    def amendEachRow(amender: Signal[Data] => Seq[Modifier[TableRow]]): Builder[Data, Key] =
      copy(rowAmender = (_, _, data) => amender(data))

    def withHeader(header: Seq[TableRow]): Builder[Data, Key] = copy(header = header)

    def withFooter(footer: Seq[TableRow]): Builder[Data, Key] = copy(footer = footer)

    def render: Table = {
      val isEmpty = datas.map(_.isEmpty)
      val nonEmpty = isEmpty.invert

      table(
        tableModifiers,
        thead(columns.flatMap(_.headerSeq)*),
        tbody(
          child.maybe <-- isEmpty.splitBooleanAsOption(_ => tr(td(colSpan := columns.size, onNoEntriesFound))),
          children <-- nonEmpty.splitBooleanOrEmpty(_ => header),
          children <-- {
            def project(key: Key, initial: Data, signal: Signal[Data]): TableRow = tr(
              rowAmender(key, initial, signal),
              columns.flatMap(_.cellSeq(signal)),
            )

            dataSplitter(datas, project)
          },
          children <-- nonEmpty.splitBooleanOrEmpty(_ => footer),
        ),
      )
    }
  }
  object Builder {
    given Conversion[Builder[?, ?], Table] = _.render
  }
}
