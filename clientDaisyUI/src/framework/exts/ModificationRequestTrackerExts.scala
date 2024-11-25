package framework.exts

import com.raquo.laminar.modifiers.Binder
import framework.components.*
import framework.data.SendSignal
import framework.sourcecode.DefinedAt
import framework.utils.{ModificationRequestTracker, NetworkOrAuthError}

trait ButtonErrorHandler[-AuthError] extends (NetworkOrAuthError[AuthError] => Unit)

trait SendButtonContents {

  /** E.g. "Form contains invalid fields." */
  def formHasInvalidFields: String

  /** Button contents for a the send button. */
  def buttonContents: ButtonContents
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
    wideButton: Boolean = true,
  )(
    callback: A => Unit
  )(using errorHandler: ButtonErrorHandler[AuthError], definedAt: DefinedAt, contents: SendButtonContents): L.Div = {
    import L.*

    Tooltip(
      sendSignal.canSendSignal.map(canSend => Option.unless(canSend)(summon[SendButtonContents].formHasInvalidFields))
    )(
      button(
        `type` := "button",
        cls := "btn",
        when(wideButton)(cls := "btn-wide"),
        cls <-- cssClasses.cssClasses(sendSignal.canSendSignal, tracker),
        disabled <-- tracker.disabledSignal(sendSignal, formIsSubmitting),
        children <-- tracker.submitting.flatMapSwitch {
          case true  => contents.buttonContents.whenSubmitting.deunionizeSignal
          case false => contents.buttonContents.whenNotSubmitting.deunionizeSignal
        },
        handleOnClick(sendSignal, tracker.canCancel, callback),
      )
    )
  }

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
  )(
    callback: A => Unit
  )(using ButtonErrorHandler[AuthError], DefinedAt): L.Button = {
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
      handleOnClick(sendSignal, tracker.canCancel, callback),
    )
  }

  def handleOnClick[AuthError, Response](
    sendSignal: SendSignal[AuthError, Response],
    canCancelSignal: Signal[Option[() => Unit]],
    callback: Response => Unit,
  )(using errorHandler: ButtonErrorHandler[AuthError], definedAt: DefinedAt): Binder[L.Element] = {
    import L.*

    onClick(_.sample(sendSignal.signal, tracker.canCancel)) --> {
      case (None, None) =>
        logError("Can't neither send or cancel, both signals are `None`, the button should be disabled.")

      case (Some(_), Some(cancel)) =>
        logError("Can't send and cancel at the same time, choosing to cancel.")
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
            logError(s"Request error: $err")
            errorHandler(err)
          case Some(ModificationRequestTracker.Result.Finished(response)) =>
            callback(response)
        }
    }
  }
}
