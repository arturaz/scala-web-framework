package framework.components

import framework.utils.ModificationRequestTracker

import L.*
import framework.data.LocalLoadingStatus
import framework.data.Email
import framework.localization.LocalizedAppliedValidate

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

/** @param htmlSignal
  *   the HTML to render
  * @param emailStrRx
  *   the raw email input
  * @param emailRx
  *   the parsed email
  * @param makeEmailInput
  *   creates the email input field
  * @param makeLoginButton
  *   creates the login button that performs the specified [[IO]] when clicked
  */
case class LoginViaEmailWithOTPResult(
  htmlSignal: Signal[Element],
  emailStrRx: Var[String],
  emailRx: Signal[Option[Email]],
  makeEmailInput: () => Seq[Modifier[Div]],
  makeLoginButton: (Email => IO[Unit]) => Button,
)

/** Login via email with one-time password (OTP).
  *
  * @param isLoggedInSignal
  *   the signal that indicates whether the user is logged in, the `String` is the email
  * @param beforeEmailInputLabel
  *   ((rawEmailVar, emailSignal) => content). The content that is shown before the email input label.
  * @param emailInputLabel
  *   the label for the email input to which OTP will be sent
  * @param emailInputModifiers
  *   (tracker => modifiers). The modifiers for the email input. If you override the `disabled` property make sure to
  *   include `disabled <-- tracker.submitting` into your new expression.
  * @param afterEmailInputLabel
  *   ((rawEmailVar, emailSignal) => content). The content that is shown after the email input label.
  * @param loginButtonContent
  *   the content for the login button
  * @param beforeOtpInputLabel
  *   ((email, sendOTPResult) => content). The content that is shown before the OTP input.
  * @param afterOtpInputLabel
  *   ((email, sendOTPResult) => content). The content that is shown after the OTP input.
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
  * @param otpInputModifiers
  *   (tracker => modifiers). The modifiers for the OTP input. If you override the `disabled` property make sure to
  *   include `disabled <-- tracker.submitting` into your new expression.
  * @param sendOTP
  *   (email => [[IO]]). Sends an OTP to the given email address.
  * @param verifyOTP
  *   ((email, otp) => [[IO]]). Verifies the given OTP for the given email address.
  */
def LoginViaEmailWithOTP[SendOTPResult](
  pageLoadingIndicator: PageLoadingIndicator,
  sendOTP: Email => IO[Either[LoginViaEmailWithOTPError, SendOTPResult]],
  verifyOTP: (Email, String, SendOTPResult) => IO[Either[LoginViaEmailWithOTPError, Boolean]],
  isLoggedInSignal: Signal[LocalLoadingStatus[Option[Email]]],
  emailInputLabel: String,
  emailInputPlaceholder: Option[String],
  emailValidation: Option[LocalizedAppliedValidate[String]],
  loginButtonContent: => Seq[Modifier[Button]],
  beforeOtpInputLabel: (Email, SendOTPResult) => Seq[Modifier[Div]],
  otpInputLabel: String,
  otpInputPlaceholder: Option[String],
  otpValidation: Option[LocalizedAppliedValidate[String]],
  otpCheckButtonContent: Seq[Modifier[Button]],
  otpVerificationFailedContent: (Email, SendOTPResult) => Seq[Modifier[Div]],
  otpVerificationSucceededContent: (Email, Option[SendOTPResult]) => Seq[Modifier[Div]],
  beforeEmailInputLabel: (Var[String], Signal[Option[Email]]) => Seq[Modifier[Div]] = (_, _) => Seq.empty,
  emailInputModifiers: ModificationRequestTracker => Seq[Modifier[Input]] = _ => Seq.empty,
  afterEmailInputLabel: (Var[String], Signal[Option[Email]]) => Seq[Modifier[Div]] = (_, _) => Seq.empty,
  otpInputModifiers: ModificationRequestTracker => Seq[Modifier[Input]] = _ => Seq.empty,
  afterOtpInputLabel: (Email, SendOTPResult) => Seq[Modifier[Div]] = (_, _: SendOTPResult) => Seq.empty,
): LoginViaEmailWithOTPResult = {

  /** The email input. */
  val emailStrRx = Var("")
  val emailRx = emailStrRx.signal.map(Email.make(_).toOption)

  /** Where the one-time password was sent. */
  val oneTimePasswordSentRx = Var(Option.empty[(Email, SendOTPResult)])

  val cannotProgressToNextStepRx = Var(Option.empty[LoginViaEmailWithOTPError.CannotProgressToNextStep])
  val fatalErrorRx = Var(Option.empty[LoginViaEmailWithOTPError.FatalError])

  def reset(): Unit = {
    oneTimePasswordSentRx.set(None)
    cannotProgressToNextStepRx.set(None)
    fatalErrorRx.set(None)
  }

  val tracker = ModificationRequestTracker()

  def makeEmailInput(): Seq[Modifier[Div]] = Seq(
    beforeEmailInputLabel(emailStrRx, emailRx),
    FormInput
      .stringWithLabel(
        emailInputLabel,
        emailStrRx,
        validation = emailValidation,
        placeholder = emailInputPlaceholder,
        inputModifiers = Seq(
          `type` := "email",
          disabled <-- tracker.submitting,
        ) ++ emailInputModifiers(tracker),
      ),
    afterEmailInputLabel(emailStrRx, emailRx),
  )

  val inputInvalid = emailValidation.fold2(
    emailRx.map(_.isEmpty),
    validation =>
      emailStrRx.signal.combineWithFn(validation.validate)((str, validate) => str.isBlank() || !validate.isValid(str)),
  )

  def makeLoginButton(onButtonClick: Email => IO[Unit]) = {
    button(
      `type` := "submit",
      cls := "btn btn-primary",
      disabled <-- tracker.submitting.combineWithFn(inputInvalid)(_ || _),
      child.maybe <-- tracker.submitting.splitBooleanAsOption(_ => Spinner),
      loginButtonContent,
      onClick(_.sample(emailRx).collectOpt(identity)) ---> onButtonClick,
    )
  }

  def otpNotSent = {
    div(
      cls := "space-y-2",
      child.maybe <-- cannotProgressToNextStepRx.signal.mapSome(err => div(err.userFriendlyMessage)),
      makeEmailInput(),
      makeLoginButton { email =>
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
    )
  }

  def otpSent(email: Email, result: SendOTPResult) = {
    val otpRx = Var("")
    val otpVerifiedRx = Var(Option.empty[Boolean])

    val inputInvalid = otpValidation.fold2(
      otpRx.signal.map(_.isEmpty),
      validation =>
        otpRx.signal.combineWithFn(validation.validate)((str, validate) => str.isBlank() || !validate.isValid(str)),
    )

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
              cls := "space-y-2",
              beforeOtpInputLabel(email, result),
              FormInput
                .stringWithLabel(
                  otpInputLabel,
                  otpRx,
                  validation = otpValidation,
                  placeholder = otpInputPlaceholder,
                  inputModifiers = Seq(disabled <-- tracker.submitting) ++ otpInputModifiers(tracker),
                ),
              afterOtpInputLabel(email, result),
              button(
                `type` := "submit",
                cls := "btn btn-primary",
                disabled <-- tracker.submitting.combineWithFn(inputInvalid)(_ || _),
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
        pending = (_, _) => Signal.fromValue(pageLoadingIndicator.html),
      )
      .flattenSwitch

  LoginViaEmailWithOTPResult(html, emailStrRx, emailRx, makeEmailInput, makeLoginButton)
}
