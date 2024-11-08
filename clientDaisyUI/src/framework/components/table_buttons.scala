package framework.components

import L.*
import framework.localization.LocalizationSupport

/** Contents for [[RemoveButton]]. */
trait RemoveButtonContents {
  def ariaLabel: Signal[String]

  def contents: L.Node
}

/** Renders a remove button for a table cell.
  *
  * @param remove
  *   if [[None]] the button is disabled.
  * @param submitting
  *   whether the form is submitting.
  * @return
  */
def RemoveButton(remove: Option[() => Unit], submitting: Signal[Boolean])(using contents: RemoveButtonContents) = {
  button(
    `type` := "button",
    cls := "btn btn-ghost btn-tiny",
    aria.label <-- contents.ariaLabel,
    remove match {
      case None => disabled := true
      case Some(remove) =>
        Vector(
          onClick --> { _ => remove() },
          disabled <-- submitting,
        )
    },
    contents.contents,
  )
}
