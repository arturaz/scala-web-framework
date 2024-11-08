package framework.components

import framework.data.MaybeSignal
import L.*
import scala.annotation.targetName

def Tooltip(tooltip: String, modifiers: Modifier[Div]*)(content: Modifier[Div]*): Div = {
  div(
    cls := "tooltip",
    modifiers,
    dataAttr("tip") := tooltip,
    content,
  )
}

def Tooltip(tooltip: Signal[String], modifiers: Modifier[Div]*)(content: Modifier[Div]*): Div = {
  div(
    cls := "tooltip",
    modifiers,
    dataAttr("tip") <-- tooltip,
    content,
  )
}

@targetName("tooltipMaybeSignal")
def Tooltip(tooltip: Signal[Option[String]], modifiers: Modifier[Div]*)(content: Modifier[Div]*): Div = {
  div(
    cls("tooltip") <-- tooltip.map(_.isDefined),
    modifiers,
    dataAttr("tip") <-- tooltip.map(_.getOrElse("")),
    content,
  )
}
