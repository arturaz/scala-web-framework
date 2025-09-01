package framework.components

import cats.syntax.all.*
import framework.data.{MaybeSignal, PageCursor, PageCursors}
import framework.utils.{NamedEnum, RouterOps, ZoomedOwnerlessSignal}

import L.*

trait PageNumberToText extends (Int => String) {
  def apply(pageNumber: Int): String
}

trait PaginationL18n {
  def previousPageText: Signal[String] = Signal.fromValue("«")
  def nextPageText: Signal[String] = Signal.fromValue("»")

  def pageNumberToText: Signal[PageNumberToText]

  /** Text for the [[PageSizeSelector]], saying "Results per page". */
  def resultsPerPageText: Signal[String]
}

/** @param pageNumberToText
  *   turns a 1-based page number into a string
  */
def Pagination[Page](
  cursors: MaybeSignal[PageCursors.OfInput[Page]]
)(using router: RouterOps.In[Page], l18n: PaginationL18n) = {
  val signal = cursors.deunionizeSignal

  div(
    cls := "join",
    child.maybe <-- signal.map(_.previousPage).splitOption { (_, signal) =>
      a(cls := "join-item btn", navigateTo(signal), child.text <-- l18n.previousPageText)
    },
    button(
      cls := "join-item btn",
      child.text <-- signal
        .map(_.pageIndex)
        .combineWithFn(l18n.pageNumberToText)((pageIndex, fn) => fn(pageIndex + 1)),
    ),
    child.maybe <-- signal.map(_.nextPage).splitOption { (_, signal) =>
      a(cls := "join-item btn", navigateTo(signal), child.text <-- l18n.nextPageText)
    },
  )
}

def PageSizeSelector[PageSize: CanEqual1](
  pageSizeSignal: ZoomedOwnerlessSignal[PageSize]
)(using pageSizeEnum: NamedEnum.WithIntRepresentation[PageSize], l18n: PaginationL18n) = {
  div(
    cls := "flex bg-base-200 rounded-lg pl-4",
    // Text aligned to the middle vertically
    div(cls := "font-medium self-center pr-4", child.text <-- l18n.resultsPerPageText),
    FormInput
      .selectElement(
        pageSizeSignal,
        pageSizeEnum.values.map(o => (o, pageSizeEnum.toInt(o).show)),
      )
      .amend(cls := "self-center w-20"),
  )
}

def PaginationWithPageSizeSelector[Page, PageSize: CanEqual1](
  cursors: MaybeSignal[PageCursors.OfInput[Page]],
  pageSizeSignal: ZoomedOwnerlessSignal[PageSize],
)(using RouterOps.In[Page], PaginationL18n)(using pageSizeEnum: NamedEnum.WithIntRepresentation[PageSize]): Div = {
  val hasDifferentPageSizes = pageSizeEnum.values.size > 1

  div(
    cls := "flex my-4 gap-10",
    cls := (if (hasDifferentPageSizes) "justify-between" else "justify-center"),
    when(hasDifferentPageSizes)(PageSizeSelector(pageSizeSignal)),
    Pagination(cursors),
  )
}

// def PaginationWithPageSizeSelector[Page, PageSize: CanEqual1](
//   pageCursors: PageCursors.Applicable[Page, ?, ?, PageSize],
//   pageSizeSignal: ZoomedOwnerlessSignal[PageSize],
// )(using RouterOps[Page], PaginationL18n)(using pageSizeEnum: NamedEnum.WithIntRepresentation[PageSize]) =
//   PaginationWithPageSizeSelector(
//     pageCursors.previousPageCursor,
//     pageCursors.currentPageCursor,
//     pageCursors.nextPageCursor,
//     pageSizeSignal,
//   )
