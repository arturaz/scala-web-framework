package framework.components

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.{HTMLTableElement, HTMLTableRowElement, HTMLTableSectionElement}

/** If you are annoyed by the design of HTML tables and that headers are separated from the body, this is the component
  * for you.
  */
object TableHelper {
  type Table = ReactiveHtmlElement[HTMLTableElement]
  type TableSection = ReactiveHtmlElement[HTMLTableSectionElement]
  type TableRow = ReactiveHtmlElement[HTMLTableRowElement]

  case class Column[-Data](
    header: Modifier[TableSection] | Seq[Modifier[TableSection]],
    cell: Data => (Modifier[TableRow] | Seq[Modifier[TableRow]]),
  ) {
    def headerSeq: Seq[Modifier[TableSection]] = header match {
      case header: Modifier[TableSection @unchecked]       => Seq(header)
      case headers: Seq[Modifier[TableSection] @unchecked] => headers
    }

    def cellSeq(data: Data): Seq[Modifier[TableRow]] = cell(data) match {
      case cell: Modifier[TableRow @unchecked]       => Seq(cell)
      case cells: Seq[Modifier[TableRow] @unchecked] => cells
    }
  }

  def apply[Data](
    datas: Seq[Data],
    columns: Seq[Column[Data]],
    tableModifiers: Modifier[Table]*
  ) = {
    table(
      tableModifiers,
      thead(columns.flatMap(_.headerSeq)*),
      tbody(
        datas.map { data =>
          tr(columns.flatMap(_.cellSeq(data)))
        }
      ),
    )
  }
}
