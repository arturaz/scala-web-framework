package framework.components

import alleycats.Empty
import framework.data.{HasSurroundingPages, PageCursor}
import framework.localization.LocalizationSupport
import framework.utils.{NamedEnum, PageDataLoader, PageRenderResult, RouterOps}

import L.*

/** An item in the admin panel list.
  *
  * @param header
  *   The header of the item.
  * @param show
  *   The content to show when the item is shown.
  * @param edit
  *   The content to show when the item is being edited. If `None`, the item cannot be edited.
  */
case class AdminPanelListItemDetail(
  header: Modifier[Div],
  show: Modifier[Div],
  edit: Option[Modifier[Div]] = None,
)

/** Renders a list of items for an admin panel. */
def AdminPanelList[
  Input,
  Response,
  PageSize,
  CursorPrimaryColumn,
  CursorSecondaryColumn,
  Collection[X] <: IndexedSeq[X],
  Element,
](
  loader: PageDataLoader#BuilderWithRequest[Input, Response],
  extractSurroundingPages: Response => HasSurroundingPages[Collection[Element]],
  cursorLens: Input => AppliedLens[Input, PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize]],
  getPrimary: Element => CursorPrimaryColumn,
  getSecondary: Element => CursorSecondaryColumn,
  onNoItems: Modifier[Div],
  editButtonContents: Modifier[Button],
  getHeader: Element => String,
  getSubHeader: Element => Option[String],
  getContents: (Element, AdminPanelListItemDetail.type) => Seq[AdminPanelListItemDetail],
)(using
  pageSizeEmpty: Empty[PageSize],
  pageSizeCanEqual: CanEqual1[PageSize],
  pageSizeEnum: NamedEnum.WithIntRepresentation[PageSize],
  router: RouterOps.In[Input],
  paginationL18n: PaginationL18n,
) = {
  loader { withInput =>
    val response = withInput.fetchedData
    val surroundingPages = extractSurroundingPages(response)
    val page = withInput.input

    val pageCursors = withInput.pageCursorsForIndexedSeq(extractSurroundingPages, cursorLens, getPrimary, getSecondary)
    val pageSizeRx = Var(empty[PageSize])

    val html =
      if (surroundingPages.isEmpty) div(onNoItems)
      else
        div(
          div( // container for all items
            cls := "shadow-lg rounded-lg mb-8 border",
            surroundingPages.data.map { item =>
              val contents = getContents(item, AdminPanelListItemDetail)
              val isEditable = contents.exists(_.edit.isDefined)

              div( // single item
                cls := "collapse collapse-arrow border border-base-200 rounded-none",
                input(`type` := "checkbox", cls := "peer"),
                div(
                  cls := "collapse-title peer-checked:bg-base-200 flex flex-row gap-4 items-baseline",
                  h3(cls := "text-md font-semibold", getHeader(item)),
                  getSubHeader(item).map(subHeader => span(cls := "text-sm opacity-50", subHeader)),
                ),
                div(
                  cls := "collapse-content border-t border-dashed",
                  div(
                    cls := "my-4 flex flex-col md:grid md:grid-cols-[auto_1fr] md:gap-4",
                    contents.iterator.zipWithIndex.flatMap { case (details, idx) =>
                      nodeSeq(
                        div(when(idx != 0)(cls := "mt-2 md:mt-0"), details.header),
                        div(details.show),
                      )
                    }.toArray,
                  ),
                  when(isEditable)(
                    div(
                      cls := "flex flex-row gap-4 justify-end",
                      button(cls := "btn btn-primary btn-sm btn-wide", editButtonContents),
                    )
                  ),
                ),
              )

            },
          ),
          PaginationWithPageSizeSelector(pageCursors, pageSizeRx),
        )

    PageRenderResult(html)
  }
}
