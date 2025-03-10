package framework.components

import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L.*
import com.raquo.laminar.modifiers.RenderableText
import com.raquo.laminar.nodes.ReactiveHtmlElement
import framework.data.MaybeSignal
import framework.utils.UpdatableSignal
import io.scalaland.chimney.Transformer
import org.scalajs.dom.{HTMLDivElement, HTMLHeadingElement}

import java.text.DecimalFormat
import scala.annotation.targetName

object FormRender {
  def card(
    title: MaybeSignal[String],
    nextToTitle: Seq[Modifier[Div]] = Seq.empty,
    cardModifiers: Seq[Modifier[Div]] = Seq.empty,
    actions: Seq[Modifier[Div]] = Seq.empty,
  )(content: Modifier[Div]*): Div = {
    div(
      cls := "card card-compact card-border bg-base-100 shadow-inner mb-3",
      cardModifiers,
      div(
        cls := "card-body",
        div(
          cls := "flex",
          h2(
            cls := "card-title",
            child.text <-- title.deunionizeSignal,
          ),
          nextToTitle,
        ),
        div(content),
      ),
      when(actions.nonEmpty)(
        div(
          cls := "card-actions justify-end pe-3 pb-3",
          actions,
        )
      ),
    )
  }
}
