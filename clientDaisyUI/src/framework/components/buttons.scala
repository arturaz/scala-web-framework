package framework.components

import L.*

trait AddButtonContents {
  def icon: Option[L.Node]

  def text: Signal[String]
}

def AddButton(onClick: () => Unit, submitting: Signal[Boolean])(using contents: AddButtonContents) = {
  button(
    `type` := "button",
    cls := "btn btn-secondary",
    disabled <-- submitting,
    L.onClick --> { _ => onClick() },
    contents.icon,
    child.text <-- contents.text,
  )
}
