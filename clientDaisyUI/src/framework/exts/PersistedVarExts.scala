package framework.exts

import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.{ChildNode, ReactiveHtmlElement}
import framework.utils.PersistedVar
import org.scalajs.dom.{window, Node, Storage}

extension [A](persisted: PersistedVar[A]) {

  /** @note
    *   the name is an ugly workaround for
    *   [[https://discordapp.com/channels/632150470000902164/632150470000902166/1248899564291887215]]
    */
  def maybeResetToDefaultButton_(
    submitting: Signal[Boolean]
  ): PersistedVarMaybeResetToDefaultButtonBuilder =
    new PersistedVarMaybeResetToDefaultButtonBuilder(persisted, submitting)
}

/** Contents for [[PersistedVarMaybeResetToDefaultButtonBuilder]]. */
trait PersistedVarMaybeResetToDefaultButtonBuilderContent {

  /** e.g. "Clear Form" */
  def buttonContents: Seq[Modifier[Button]]

  /** e.g. "Are you sure you want to clear the form?" */
  def confirmation: String
}

class PersistedVarMaybeResetToDefaultButtonBuilder(
  persistedVar: PersistedVar[?],
  submitting: Signal[Boolean],
) {
  def apply()(using content: PersistedVarMaybeResetToDefaultButtonBuilderContent): Signal[Option[Button]] = {
    persistedVar.maybeDifferentFromDefaultSignal
      .combineWithFn(submitting) {
        case (_, true) | (None, _) => None
        case (Some(_), false)      => Some(())
      }
      .distinct
      .mapSome { _ =>
        button(
          `type` := "button",
          cls := "btn btn-secondary",
          onClick --> { _ =>
            if (window.confirm(content.confirmation)) {
              persistedVar.resetToDefault()
            }
          },
          content.buttonContents,
        )
      }
  }
}
