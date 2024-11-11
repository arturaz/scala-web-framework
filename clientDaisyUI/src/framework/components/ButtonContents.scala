package framework.components

import framework.data.MaybeSignal

case class ButtonContents(
  whenSubmitting: MaybeSignal[Seq[L.Node]],
  whenNotSubmitting: MaybeSignal[Seq[L.Node]],
)
object ButtonContents {
  import L.*

  /** @param sendText
    *   The text when you can send the request
    * @param sendingText
    *   The text when you are sending the request
    * @param cancelText
    *   The small text when you are sending the request
    * @param icon
    *   Can be [[L.emptyNode]] if there is no icon.
    */
  def default(
    sendText: String,
    sendingText: String,
    cancelText: String,
    icon: Node,
  ): ButtonContents = apply(
    whenSubmitting = Vector(
      Spinner,
      div(
        p(sendingText),
        p(cls := "text-xs", cancelText),
      ),
    ),
    whenNotSubmitting = Vector(icon, textToTextNode(sendText)),
  )
}
