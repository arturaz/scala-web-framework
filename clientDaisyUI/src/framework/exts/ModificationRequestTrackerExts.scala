package framework.exts

import cats.Functor
import com.raquo.laminar.modifiers.Binder
import com.raquo.laminar.nodes.ReactiveHtmlElement
import framework.components.*
import framework.data.{MaybeSignal, SendSignal}
import framework.sourcecode.DefinedAt
import framework.utils.{ModificationRequestTracker, NetworkOrAuthError}
import org.scalajs.dom.HTMLButtonElement

trait ButtonErrorHandler[-AuthError] extends (NetworkOrAuthError[AuthError] => Unit)

/** Contains the contents of the send button.
  *
  * @param formHasInvalidFields
  *   When [[Some]] a tooltip is shown if the send button is disabled. E.g. "Form contains invalid fields."
  * @param buttonContents
  *   Button contents for a the send button.
  */
case class SendButtonContents(
  formHasInvalidFields: MaybeSignal[Option[String]],
  buttonContents: ButtonContents,
)
object SendButtonContents {
  def apply(
    formHasInvalidFields: MaybeSignal[String],
    buttonContents: ButtonContents,
    showTooltip: MaybeSignal[Boolean] = true,
  ): SendButtonContents = {
    def noneSignal = Signal.fromValue(Option.empty[String])
    def invalidFieldsSignal = formHasInvalidFields.deunionizeSignal.map(Some(_))

    apply(
      formHasInvalidFields = showTooltip.fold(
        ifValue = {
          case false => noneSignal
          case true  => invalidFieldsSignal
        },
        ifSignal = _.flatMapSwitch {
          case false => noneSignal
          case true  => invalidFieldsSignal
        },
      ),
      buttonContents,
    )
  }

}

extension (tracker: ModificationRequestTracker) {

  /** Returns a [[Signal]] that tells whether the button should be disabled. */
  def disabledSignal(
    sendSignal: SendSignal[?, ?],
    formIsSubmitting: Signal[Boolean],
  ): Signal[Boolean] =
    formIsSubmitting.combineWithFn(sendSignal.canSendSignal, tracker.canCancelBool)(
      (formIsSubmitting, canSend, canCancel) =>
        // The button is disabled if:
        // - the form is submitting but we can't cancel, which means some other button is currently submitting
        // - the form is not submitting and we can't send (probably because some validation does not pass)
        (formIsSubmitting && !canCancel) || !canSend
    )

  /** The send button for this tracker.
    *
    * @param formIsSubmitting
    *   whether any action in the form is in progress. You probably want to get it from [[EditForm.submitting]].
    * @param wideButton
    *   whether the button should be wide.
    */
  def sendButton[AuthError, A](
    sendSignal: SendSignal[AuthError, A],
    formIsSubmitting: Signal[Boolean],
    cssClasses: SendCancelButtonCssClasses = SendCancelButtonCssClasses.default,
    wideButton: MaybeSignal[Boolean] = true,
    buttonModifiers: Seq[L.Modifier[ReactiveHtmlElement[HTMLButtonElement]]] = Seq.empty,
  )(using
    errorHandler: ButtonErrorHandler[AuthError],
    definedAt: DefinedAt,
    contents: SendButtonContents,
  ): SendButtonBuilder[AuthError, A] = SendButtonBuilder(
    tracker,
    sendSignal,
    formIsSubmitting,
    cssClasses,
    wideButton,
    buttonModifiers,
    errorHandler,
    definedAt,
    contents,
  )

  /** The delete button for this tracker.
    *
    * @param formIsSubmitting
    *   whether any action in the form is in progress. You probably want to get it from [[EditForm.submitting]].
    */
  def deleteButton[AuthError, A](
    sendSignal: SendSignal[AuthError, A],
    formIsSubmitting: Signal[Boolean],
    contents: ButtonContents,
    cssClasses: SendCancelButtonCssClasses = SendCancelButtonCssClasses.dangerous,
  )(using errorHandler: ButtonErrorHandler[AuthError], definedAt: DefinedAt): DeleteButtonBuilder[AuthError, A] =
    DeleteButtonBuilder(
      tracker,
      sendSignal,
      formIsSubmitting,
      contents,
      cssClasses,
      errorHandler,
      definedAt,
    )

  def handleOnClick[AuthError, Response](
    sendSignal: SendSignal[AuthError, Response],
    canCancelSignal: Signal[Option[() => Unit]],
    callback: Response => Unit,
  )(using errorHandler: ButtonErrorHandler[AuthError], definedAt: DefinedAt): Binder[L.Element] = {
    import L.*

    onClick(_.sample(sendSignal.signal, tracker.canCancel)) --> {
      case (None, None) =>
        log.error("Can't neither send or cancel, both signals are `None`, the button should be disabled.")

      case (Some(_), Some(cancel)) =>
        log.error("Can't send and cancel at the same time, choosing to cancel.")
        cancel()

      case (None, Some(cancel)) =>
        cancel()

      case (Some(send), None) =>
        OptionT(send.to[IO]).semiflatMap(tracker.launch).value.unsafeRunAsyncOrHandleError {
          case None =>
            log("User cancelled the action.")
          case Some(ModificationRequestTracker.Result.Cancelled) =>
            log("Request cancelled.")
          case Some(ModificationRequestTracker.Result.Error(err)) =>
            log.error(s"Request error: $err")
            errorHandler(err)
          case Some(ModificationRequestTracker.Result.Finished(response)) =>
            callback(response)
        }
    }
  }
}

case class SendButtonBuilder[AuthError, Response](
  tracker: ModificationRequestTracker,
  sendSignal: SendSignal[AuthError, Response],
  formIsSubmitting: Signal[Boolean],
  cssClasses: SendCancelButtonCssClasses,
  wideButton: MaybeSignal[Boolean],
  buttonModifiers: Seq[L.Modifier[ReactiveHtmlElement[HTMLButtonElement]]],
  errorHandler: ButtonErrorHandler[AuthError],
  definedAt: DefinedAt,
  contents: SendButtonContents,
) extends SendButtonBuilder.ToResult[Response] {
  override def apply(callback: Response => Unit): SendButtonResult = {
    import L.*

    val btn = button(
      `type` := "button",
      cls := "btn",
      wideButton.fold(cls("btn-wide") <-- _, when(_)(cls := "btn-wide")),
      cls <-- cssClasses.cssClasses(sendSignal.canSendSignal, tracker),
      disabled <-- tracker.disabledSignal(sendSignal, formIsSubmitting),
      children <-- tracker.submitting.flatMapSwitch {
        case true  => contents.buttonContents.whenSubmitting.deunionizeSignal
        case false => contents.buttonContents.whenNotSubmitting.deunionizeSignal
      },
      tracker.handleOnClick(sendSignal, tracker.canCancel, callback)(using errorHandler, definedAt),
      buttonModifiers,
    )

    SendButtonResult(
      btn => {
        val canNotSendSignal = sendSignal.canSendSignal.invert
        val showTooltip = contents.formHasInvalidFields.fold(
          _.combineWithFn(canNotSendSignal)((opt, canNotSend) => opt.filter(_ => canNotSend)),
          maybeMsg => canNotSendSignal.map(canNotSend => maybeMsg.filter(_ => canNotSend)),
        )
        Tooltip(showTooltip)(btn)
      },
      btn,
    )
  }
}
object SendButtonBuilder {
  trait ToResult[Response] {
    def apply(callback: Response => Unit): SendButtonResult

    def map[Response2](f: Response => Response2): ToResult[Response2] =
      callback => apply(response => callback(f(response)))
  }
  object ToResult {
    given functor: Functor[ToResult] with {
      override def map[A, B](fa: ToResult[A])(f: A => B): ToResult[B] = fa.map(f)
    }
  }
}

case class SendButtonResult(makeOuter: L.Button => L.Div, inner: L.Button) {
  def outer: L.Div = makeOuter(inner)

  def modButton(f: L.Button => L.Button): SendButtonResult = copy(inner = f(inner))
}
object SendButtonResult {
  given Conversion[SendButtonResult, L.Div] = _.outer
}

case class DeleteButtonBuilder[AuthError, Response](
  tracker: ModificationRequestTracker,
  sendSignal: SendSignal[AuthError, Response],
  formIsSubmitting: Signal[Boolean],
  contents: ButtonContents,
  cssClasses: SendCancelButtonCssClasses,
  errorHandler: ButtonErrorHandler[AuthError],
  definedAt: DefinedAt,
) extends DeleteButtonBuilder.ToResult[Response] {
  override def apply(callback: Response => Unit): L.Button = {
    import L.*

    button(
      `type` := "button",
      cls := "btn",
      cls <-- cssClasses.cssClasses(sendSignal.canSendSignal, tracker),
      disabled <-- tracker.disabledSignal(sendSignal, formIsSubmitting),
      children <-- tracker.submitting.flatMapSwitch {
        case true  => contents.whenSubmitting.deunionizeSignal
        case false => contents.whenNotSubmitting.deunionizeSignal
      },
      tracker.handleOnClick(sendSignal, tracker.canCancel, callback)(using errorHandler, definedAt),
    )
  }
}
object DeleteButtonBuilder {
  trait ToResult[Response] {
    def apply(callback: Response => Unit): L.Button

    def map[Response2](f: Response => Response2): ToResult[Response2] =
      callback => apply(response => callback(f(response)))
  }
  object ToResult {
    given functor: Functor[ToResult] with {
      override def map[A, B](fa: ToResult[A])(f: A => B): ToResult[B] = fa.map(f)
    }
  }
}
