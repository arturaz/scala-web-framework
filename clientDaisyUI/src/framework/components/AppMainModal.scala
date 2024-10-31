package framework.components

import com.raquo.laminar.nodes.ReactiveHtmlElement
import framework.sourcecode.DefinedAt
import org.scalajs.dom.{HTMLButtonElement, HTMLDialogElement}

import scala.annotation.implicitNotFound

import L.*

object AppMainModal {
  trait Data {
    def definedAt: DefinedAt
  }
}
@implicitNotFound(
  "Cannot find an implicit instance of `AppMainModal`, if you have `AppPageInit` in scope you want to " +
    "`import appPageInit.appMainModal`."
)
abstract class AppMainModal[Data <: AppMainModal.Data] {
  protected val dataVar: Var[Option[Data]] = Var(Option.empty[Data])

  def show(data: Data): Unit = dataVar.set(Some(data))

  /** Turns [[Data]] into HTML title. */
  def renderDataAsTitle(data: Data): Seq[Node]

  /** Turns [[Data]] into HTML content. */
  def renderDataAsContent(data: Data): Seq[Node]

  /** Renders the given string as details. */
  def detailsDiv(details: String, title: Seq[Modifier[Div]]): Div = div(
    cls := "collapse collapse-arrow bg-base-200",
    input(`type` := "checkbox"),
    div(cls := "collapse-title font-bold", title),
    div(cls := "collapse-content", pre(cls := "my-0", code(details))),
  )

  /** Contents of the "Close" button. */
  def closeButtonModifiers: Seq[Modifier[ReactiveHtmlElement[HTMLButtonElement]]]

  val tag: ReactiveHtmlElement[HTMLDialogElement] = {
    dialogTag(
      cls := "modal",
      div(
        cls := "modal-box",
        h3(
          cls := "font-bold text-lg flex items-center gap-2",
          children <-- dataVar.signal.map {
            case Some(data) => renderDataAsTitle(data)
            case None       => List(textToTextNode("You Should Not Be Seeing This"))
          },
        ),
        div(
          cls := "py-4 prose",
          children <-- dataVar.signal.map {
            case Some(data) => renderDataAsContent(data)
            case None       => List(p("If you are seeing this, something has gone wrong."))
          },
        ),
        div(
          cls := "modal-action",
          form(
            method := "dialog",
            button(cls := "btn btn-primary", closeButtonModifiers),
          ),
        ),
      ),
      dataVar.signal --> { dataOpt =>
        dataOpt match {
          case None =>
            log("Closing modal.")
            tag.ref.close()

          case Some(data) =>
            log(s"Opening modal with data: $data.")(using data.definedAt)
            tag.ref.showModal()
        }
      },
    )
  }
}
