package framework.exts

import com.raquo.laminar.api.L
import com.raquo.laminar.nodes.{ReactiveElement, ReactiveHtmlElement}
import framework.forms.EditForm
import org.scalajs.dom.HTMLDivElement

extension [A](form: EditForm.Persisted[A]) {

  /** Button that will reset the form to the default value. */
  def maybeResetToDefaultButton: PersistedVarMaybeResetToDefaultButtonBuilder =
    form.underlying.maybeResetToDefaultButton_(form.submitting)
}

extension [TVar[_], A](form: EditForm[TVar, A]) {

  /** The `div` that holds the form buttons. */
  def formButtonsHolder(contents: L.Modifier[L.Element]*): ReactiveHtmlElement[HTMLDivElement] = {
    import L.*

    div(
      cls := "flex justify-end gap-4 mt-4",
      contents,
    )
  }
}
