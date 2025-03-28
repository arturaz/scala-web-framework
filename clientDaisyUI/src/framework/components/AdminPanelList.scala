package framework.components

import alleycats.Empty
import framework.data.{HasSurroundingPages, PageCursor}
import framework.localization.LocalizationSupport
import framework.utils.{NamedEnum, PageDataLoader, PageRenderResult, RouterOps}

import L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.HTMLHeadingElement
import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.Transformer
import scala.annotation.targetName
import framework.api.RequestSuccessful
import framework.utils.FetchRequest

/** A single property of anitem in the admin panel list.
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
  edit: Option[AdminPanelListItemDetail.EditingContents] = None,
)
object AdminPanelListItemDetail {
  def edit: EditingContents.type = EditingContents

  case class EditingContents(
    kind: TextKind,
    contents: Modifier[Div],
  )
}

case class AdminPanelListItemEditing[Element, Input](
  editButton: Modifier[Button],
  cancelEditButton: Modifier[Button],
  saveButton: SendButtonBuilder.ToResult[FetchRequest.WithInput[Input, RequestSuccessful]],
  applyInputToElement: (Input, Element) => Element,
)

case class AdminPanelListItem[Element, Input](
  details: Seq[AdminPanelListItemDetail],
  edit: Option[AdminPanelListItemEditing[Element, Input]],
)
object AdminPanelListItem {
  def detail: AdminPanelListItemDetail.type = AdminPanelListItemDetail
  def edit: AdminPanelListItemEditing.type = AdminPanelListItemEditing
}

/** Renders a list of items for an admin panel. */
def AdminPanelList[
  Input,
  Response,
  PageSize,
  CursorPrimaryColumn,
  CursorSecondaryColumn,
  Collection[X] <: IndexedSeq[X],
  Element,
  UpdateRequestInput,
](
  loader: PageDataLoader.BuilderWithRequest[Input, Response],
  extractSurroundingPages: Response => HasSurroundingPages[Collection[Element]],
  cursorLens: Input => AppliedLens[Input, PageCursor[CursorPrimaryColumn, CursorSecondaryColumn, PageSize]],
  getPrimary: Element => CursorPrimaryColumn,
  getSecondary: Element => CursorSecondaryColumn,
  onNoItems: Modifier[Div],
  getHeader: StrictSignal[Element] => Modifier[ReactiveHtmlElement[HTMLHeadingElement]],
  getSubHeaders: StrictSignal[Element] => Seq[Modifier[Span]],
  getContents: (StrictSignal[Element], AdminPanelListItem.type) => AdminPanelListItem[Element, UpdateRequestInput],
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
              val itemRx = Var(item)
              val editingRx = Var(false)
              val showingSig = editingRx.signal.invert

              val contents = getContents(itemRx.signal, AdminPanelListItem)
              val isEditable = contents.edit.isDefined && contents.details.exists(_.edit.isDefined)

              div( // single item
                cls := "collapse collapse-arrow border border-base-200 rounded-none",
                input(`type` := "checkbox", cls := "peer"),
                div(
                  cls := "collapse-title peer-checked:bg-base-200 flex flex-row gap-4 items-baseline",
                  h3(cls := "text-md font-semibold", getHeader(itemRx.signal)),
                  getSubHeaders(itemRx.signal).map(subHeader => span(cls := "text-sm opacity-50", subHeader)),
                ),
                div(
                  cls := "collapse-content border-t border-dashed",
                  div( // contents
                    cls := "my-4 flex flex-col md:grid md:grid-cols-[auto_1fr] md:gap-4",
                    contents.details.iterator.zipWithIndex.flatMap { case (details, idx) =>
                      modSeq(
                        div(
                          when(idx != 0)(cls := "mt-2 md:mt-0"),
                          when(details.edit.exists(_.kind == TextKind.SingleLine))(cls := "md:self-center"),
                          details.header,
                        ),
                        // If the item is being edited, show the edit content.
                        details.edit.map { edit =>
                          child.maybe <-- editingRx.signal.splitBooleanAsOption(_ => div(edit.contents))
                        },
                        // If the item is not editable, always show the regular content.
                        if (details.edit.isEmpty) div(details.show)
                        else child.maybe <-- showingSig.splitBooleanAsOption(_ => div(details.show)),
                      )
                    }.toArray,
                  ),
                  contents.edit.filter(_ => isEditable).map { editing => // edit buttons
                    div(
                      cls := "flex flex-row gap-4 justify-end",
                      child <-- editingRx.signal.splitBoolean(
                        _ =>
                          div(
                            cls := "join",
                            button(
                              cls := "join-item btn btn-secondary btn-sm btn-outline",
                              editing.cancelEditButton,
                              onClick --> { _ => editingRx.set(false) },
                            ),
                            editing
                              .saveButton { withInput =>
                                if (withInput.fetchedData) {
                                  itemRx.update(elem => editing.applyInputToElement(withInput.input, elem))
                                  editingRx.set(false)
                                } else () // TODO
                              }
                              .modButton(_.amend(cls := "join-item btn-primary btn-sm")),
                          ),
                        _ =>
                          button(
                            cls := "btn btn-primary btn-sm btn-wide",
                            editing.editButton,
                            onClick --> { _ => editingRx.set(true) },
                          ),
                      ),
                    )
                  },
                ),
              )

            },
          ),
          PaginationWithPageSizeSelector(pageCursors, pageSizeRx),
        )

    PageRenderResult(html)
  }
}
