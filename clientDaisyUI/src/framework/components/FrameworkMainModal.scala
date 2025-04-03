package framework.components

import com.raquo.laminar.nodes.ReactiveHtmlElement
import framework.sourcecode.DefinedAt
import framework.utils.{NewtypeBoolean, WithLogger}
import org.scalajs.dom.{HTMLButtonElement, HTMLDialogElement}

import scala.collection.immutable.Queue
import scala.concurrent.{Future, Promise}

import L.*

object FrameworkMainModal {
  trait Data {
    def definedAt: DefinedAt
  }
}
abstract class FrameworkMainModal[Data <: FrameworkMainModal.Data] extends WithLogger {
  case class DataBox(data: Data, closed: Promise[Unit])

  protected val dataVar: Var[Queue[DataBox]] = Var(Queue.empty[DataBox]).setDisplayName("AppMainModal")
  protected val currentDataSignal: Signal[Option[DataBox]] = dataVar.signal.map(_.headOption).distinct

  /** Shows a modal. If there is already a modal, this one will be shown after the previous one is closed.
    *
    * @return
    *   A future that completes when the modal is closed.
    */
  def showFuture(data: Data): Future[Unit] = {
    val promise = Promise[Unit]()
    val box = DataBox(data, promise)
    dataVar.update(q => q.enqueue(box))
    promise.future
  }

  /** Shows a modal. If there is already a modal, this one will be shown after the previous one is closed. */
  def show(data: Data): Unit = {
    val _ = showFuture(data)
  }

  /** Closes the current modal, switching to the next one, if any. */
  def close(): Unit = dataVar.update(_.tail)

  /** Closes all modals. */
  def closeAll(): Unit = dataVar.set(Queue.empty)

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
          children <-- currentDataSignal.map {
            case Some(box) => renderDataAsTitle(box.data)
            case None      => Nil // These are briefly seen while closing animation plays
          },
        ),
        div(
          cls := "py-4 prose",
          children <-- currentDataSignal.map {
            case Some(box) => renderDataAsContent(box.data)
            case None      => Nil // These are briefly seen while closing animation plays
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
      currentDataSignal.changedValues --> { case (previousDataOpt, dataOpt) =>
        previousDataOpt.flatten.foreach { box =>
          log.debug(s"modal for data closed: ${box.data}")(using box.data.definedAt)
          box.closed.success(())
        }

        dataOpt match {
          case None =>
            log.debug("closing modal.")
            tag.ref.close()

          case Some(DataBox(data, _)) =>
            log.debug(s"opening modal with data: $data.")(using data.definedAt)
            tag.ref.showModal()
        }
      },
      eventProp("close") --> { _ =>
        log.debug("modal closed.")
        close()
      },
    )
  }
}
