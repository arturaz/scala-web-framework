package framework.components

import framework.utils.ModificationRequestTracker

import L.*
import framework.data.LocalLoadingStatus

/** @param title
  *   the title of the section
  * @param details
  *   the details shown in a code block
  */
case class LoginViaEmailWithOTPErrorTechnicalDetails(title: String, details: String)

enum LoginViaEmailWithOTPError {

  /** Shown to the user. */
  def userFriendlyMessage: Seq[Modifier[Div]]

  /** Unrecoverable error has occurred.
    *
    * @param technicalDetails
    *   if [[Some]] shown as an expandable section of code.
    */
  case FatalError(
    userFriendlyMessage: Seq[Modifier[Div]],
    technicalDetails: Option[LoginViaEmailWithOTPErrorTechnicalDetails],
  )

  /** The user can progress to the next step and should stay on the current step with the message shown. */
  case CannotProgressToNextStep(userFriendlyMessage: Seq[Modifier[Div]])
}

/** Login via email with one-time password (OTP).
  *
  * @param isLoggedInSignal
  *   the signal that indicates whether the user is logged in, the `String` is the email
  * @param emailInputLabel
  *   the label for the email input to which OTP will be sent
  * @param loginButtonContent
  *   the content for the login button
  * @param beforeOtpInputLabel
  *   ((email, sendOTPResult) => content). The content that is shown before the OTP input.
  * @param otpInputLabel
  *   the label for the input where user enters the OTP
  * @param otpCheckButtonContent
  *   the content for the button that checks the OTP
  * @param otpVerificationFailedContent
  *   ((email, sendOTPResult) => content). The content that is shown when OTP verification fails.
  * @param optVerificationSucceededContent
  *   ((email, maybeSendOTPResult) => content). The content that is shown when OTP verification succeeds. The
  *   `maybeSendOTPResult` is `None` if the user is logged in without perform an OTP login, as in the case that a
  *   previous session was restored.
  * @param sendOTP
  *   (email => [[IO]]). Sends an OTP to the given email address.
  * @param verifyOTP
  *   ((email, otp) => [[IO]]). Verifies the given OTP for the given email address.
  */
def LoginViaEmailWithOTP[SendOTPResult](
  sendOTP: String => IO[Either[LoginViaEmailWithOTPError, SendOTPResult]],
  verifyOTP: (String, String, SendOTPResult) => IO[Either[LoginViaEmailWithOTPError, Boolean]],
  isLoggedInSignal: Signal[LocalLoadingStatus[Option[String]]],
  emailInputLabel: String,
  emailInputPlaceholder: Option[String],
  loginButtonContent: Seq[Modifier[Button]],
  beforeOtpInputLabel: (String, SendOTPResult) => Seq[Modifier[Div]],
  otpInputLabel: String,
  otpInputPlaceholder: Option[String],
  otpCheckButtonContent: Seq[Modifier[Button]],
  otpVerificationFailedContent: (String, SendOTPResult) => Seq[Modifier[Div]],
  otpVerificationSucceededContent: (String, Option[SendOTPResult]) => Seq[Modifier[Div]],
): Signal[Element] = {

  /** The email input. */
  val emailRx = Var("")

  /** Where the one-time password was sent. */
  val oneTimePasswordSentRx = Var(Option.empty[(String, SendOTPResult)])

  val cannotProgressToNextStepRx = Var(Option.empty[LoginViaEmailWithOTPError.CannotProgressToNextStep])
  val fatalErrorRx = Var(Option.empty[LoginViaEmailWithOTPError.FatalError])

  def reset(): Unit = {
    oneTimePasswordSentRx.set(None)
    cannotProgressToNextStepRx.set(None)
    fatalErrorRx.set(None)
  }

  val tracker = ModificationRequestTracker()

  def otpNotSent = {
    div(
      child.maybe <-- cannotProgressToNextStepRx.signal.mapSome(err => div(err.userFriendlyMessage)),
      FormInput
        .stringWithLabel(
          emailInputLabel,
          emailRx,
          validation = None,
          placeholder = emailInputPlaceholder,
          inputModifiers = Seq(disabled <-- tracker.submitting),
        ),
      button(
        `type` := "submit",
        cls := "btn btn-primary",
        cls("btn-disabled") <-- tracker.submitting,
        child.maybe <-- tracker.submitting.splitBooleanAsOption(_ => Spinner),
        loginButtonContent,
        onClick(_.sample(emailRx.signal)) ---> { email =>
          tracker
            .launch(EitherT(sendOTP(email)))
            .flatMap {
              case ModificationRequestTracker.Result.Cancelled => IO.unit
              case ModificationRequestTracker.Result.Error(err: LoginViaEmailWithOTPError.FatalError) =>
                IO(fatalErrorRx.set(Some(err)))
              case ModificationRequestTracker.Result.Error(err: LoginViaEmailWithOTPError.CannotProgressToNextStep) =>
                IO(cannotProgressToNextStepRx.set(Some(err)))
              case ModificationRequestTracker.Result.Finished(result) =>
                IO {
                  cannotProgressToNextStepRx.set(None)
                  oneTimePasswordSentRx.set(Some((email, result)))
                }
            }
        },
      ),
    )
  }

  def otpSent(email: String, result: SendOTPResult) = {
    val otpRx = Var("")
    val otpVerifiedRx = Var(Option.empty[Boolean])

    div(
      child.maybe <-- otpVerifiedRx.signal
        .map(_.contains(false))
        .splitBooleanAsOption(_ => div(otpVerificationFailedContent(email, result))),
      child.maybe <-- cannotProgressToNextStepRx.signal.mapSome(err => div(err.userFriendlyMessage)),
      child <-- otpVerifiedRx.signal
        .map(_.contains(true))
        .splitBoolean(
          whenFalse = _ =>
            div(
              beforeOtpInputLabel(email, result),
              FormInput
                .stringWithLabel(
                  otpInputLabel,
                  otpRx,
                  validation = None,
                  placeholder = otpInputPlaceholder,
                  inputModifiers = Seq(disabled <-- tracker.submitting),
                ),
              button(
                `type` := "submit",
                cls := "btn btn-primary",
                cls("btn-disabled") <-- tracker.submitting,
                child.maybe <-- tracker.submitting.splitBooleanAsOption(_ => Spinner),
                otpCheckButtonContent,
                onClick(_.sample(otpRx.signal)) ---> { otp =>
                  tracker
                    .launch(EitherT(IO(otpVerifiedRx.set(None)) *> verifyOTP(email, otp, result)))
                    .flatMap {
                      case ModificationRequestTracker.Result.Cancelled => IO.unit
                      case ModificationRequestTracker.Result.Error(err: LoginViaEmailWithOTPError.FatalError) =>
                        IO(fatalErrorRx.set(Some(err)))
                      case ModificationRequestTracker.Result
                            .Error(err: LoginViaEmailWithOTPError.CannotProgressToNextStep) =>
                        IO(cannotProgressToNextStepRx.set(Some(err)))
                      case ModificationRequestTracker.Result.Finished(verified) =>
                        IO {
                          cannotProgressToNextStepRx.set(None)
                          otpVerifiedRx.set(Some(verified))
                        }
                    }
                },
              ),
            ),
          whenTrue = _ => div(otpVerificationSucceededContent(email, Some(result))),
        ),
    )
  }

  def onFatalError(err: Signal[LoginViaEmailWithOTPError.FatalError]) = {
    div(
      child <-- err.map(err => div(err.userFriendlyMessage)),
      child.maybe <-- err.map(_.technicalDetails).mapSome { details =>
        div(
          cls := "collapse collapse-arrow bg-base-200 mt-4",
          input(`type` := "checkbox"),
          div(cls := "collapse-title font-bold", details.title),
          div(cls := "collapse-content", pre(cls := "my-0", code(details.details))),
        )
      },
    )
  }

  val html =
    isLoggedInSignal
      .map(_.toAirstream())
      .splitStatus(
        resolved = (_, maybeEmailRx) =>
          maybeEmailRx
            .map(_.output)
            .splitOption(
              (_, emailRx) => emailRx.map(email => div(otpVerificationSucceededContent(email, None))),
              fatalErrorRx.signal
                .splitOption(
                  (_, errorRx) => onFatalError(errorRx),
                  form(
                    onSubmit --> { evt => evt.preventDefault() },
                    child <--
                      oneTimePasswordSentRx.signal
                        .splitOption(
                          (_, oneTimePasswordSentRx) => oneTimePasswordSentRx.map(otpSent),
                          Signal.fromValue(otpNotSent),
                        )
                        .flattenSwitch,
                  ),
                ),
            )
            .flattenSwitch,
        pending = (_, _) => Signal.fromValue(PageLoadingIndicatorSkeleton.html),
      )
      .flattenSwitch

  html
}
