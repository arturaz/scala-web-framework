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

/** @param userFriendlyMessage
  *   shown to the user
  * @param technicalDetails
  *   if [[Some]] shown as an expandable section of code.
  */
case class LoginViaEmailWithOTPError(
  userFriendlyMessage: Seq[Modifier[Div]],
  technicalDetails: Option[LoginViaEmailWithOTPErrorTechnicalDetails],
)

/** Login via email with one-time password (OTP).
  *
  * @param emailInputLabel
  *   the label for the email input to which OTP will be sent
  * @param loginButtonContent
  *   the content for the login button
  * @param beforeOtpInputLabel
  *   (email => content). The content that is shown before the OTP input.
  * @param otpInputLabel
  *   the label for the input where user enters the OTP
  * @param otpCheckButtonContent
  *   the content for the button that checks the OTP
  * @param otpVerificationFailedContent
  *   (email => content). The content that is shown when OTP verification fails.
  * @param optVerificationSucceededContent
  *   (email => content). The content that is shown when OTP verification succeeds.
  * @param sendOTP
  *   (email => [[IO]]). Sends an OTP to the given email address.
  * @param verifyOTP
  *   ((email, otp) => [[IO]]). Verifies the given OTP for the given email address.
  */
def LoginViaEmailWithOTP(
  isLoggedInSignal: Signal[LocalLoadingStatus[Option[String]]],
  emailInputLabel: String,
  emailInputPlaceholder: Option[String],
  loginButtonContent: Seq[Modifier[Button]],
  beforeOtpInputLabel: String => Seq[Modifier[Div]],
  otpInputLabel: String,
  otpInputPlaceholder: Option[String],
  otpCheckButtonContent: Seq[Modifier[Button]],
  otpVerificationFailedContent: String => Seq[Modifier[Div]],
  otpVerificationSucceededContent: String => Seq[Modifier[Div]],
  sendOTP: String => IO[Either[LoginViaEmailWithOTPError, Unit]],
  verifyOTP: (String, String) => IO[Either[LoginViaEmailWithOTPError, Boolean]],
): Signal[Element] = {
  val oneTimePasswordSentRx = Var(Option.empty[String])
  val emailRx = Var("")
  val errorRx = Var(Option.empty[LoginViaEmailWithOTPError])

  def reset(): Unit = {
    oneTimePasswordSentRx.set(None)
    errorRx.set(None)
  }

  val tracker = ModificationRequestTracker()

  def otpNotSent = {
    div(
      FormInput
        .stringWithLabel(
          emailInputLabel,
          emailRx,
          validation = None,
          placeholder = emailInputPlaceholder,
          inputModifiers = Seq(disabled <-- tracker.submitting),
        ),
      button(
        `type` := "button",
        cls := "btn btn-primary",
        cls("btn-disabled") <-- tracker.submitting,
        child.maybe <-- tracker.submitting.splitBooleanAsOption(_ => Spinner),
        loginButtonContent,
        onClick(_.sample(emailRx.signal)) ---> { email =>
          tracker
            .launch(EitherT(sendOTP(email)))
            .flatMap {
              case ModificationRequestTracker.Result.Cancelled    => IO.unit
              case ModificationRequestTracker.Result.Error(err)   => IO(errorRx.set(Some(err)))
              case ModificationRequestTracker.Result.Finished(()) => IO(oneTimePasswordSentRx.set(Some(email)))
            }
        },
      ),
    )
  }

  def otpSent(email: String) = {
    val otpRx = Var("")
    val otpVerifiedRx = Var(Option.empty[Boolean])

    div(
      child.maybe <-- otpVerifiedRx.signal
        .map(_.contains(false))
        .splitBooleanAsOption(_ => div(otpVerificationFailedContent(email))),
      child <-- otpVerifiedRx.signal
        .map(_.contains(true))
        .splitBoolean(
          whenFalse = _ =>
            div(
              beforeOtpInputLabel(email),
              FormInput
                .stringWithLabel(
                  otpInputLabel,
                  otpRx,
                  validation = None,
                  placeholder = otpInputPlaceholder,
                  inputModifiers = Seq(disabled <-- tracker.submitting),
                ),
              button(
                `type` := "button",
                cls := "btn btn-primary",
                cls("btn-disabled") <-- tracker.submitting,
                child.maybe <-- tracker.submitting.splitBooleanAsOption(_ => Spinner),
                otpCheckButtonContent,
                onClick(_.sample(otpRx.signal)) ---> { otp =>
                  tracker
                    .launch(EitherT(IO(otpVerifiedRx.set(None)) *> verifyOTP(email, otp)))
                    .flatMap {
                      case ModificationRequestTracker.Result.Cancelled          => IO.unit
                      case ModificationRequestTracker.Result.Error(err)         => IO(errorRx.set(Some(err)))
                      case ModificationRequestTracker.Result.Finished(verified) => IO(otpVerifiedRx.set(Some(verified)))
                    }
                },
              ),
            ),
          whenTrue = _ => div(otpVerificationSucceededContent(email)),
        ),
    )
  }

  def onError(err: LoginViaEmailWithOTPError) = {
    div(
      div(err.userFriendlyMessage),
      err.technicalDetails.map { details =>
        div(
          cls := "collapse collapse-arrow bg-base-200",
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
              (_, emailRx) => emailRx.map(email => div(otpVerificationSucceededContent(email))),
              errorRx.signal
                .splitOption(
                  (_, errorRx) => errorRx.map(onError),
                  oneTimePasswordSentRx.signal
                    .splitOption(
                      (_, oneTimePasswordSentRx) => oneTimePasswordSentRx.map(otpSent),
                      Signal.fromValue(otpNotSent),
                    )
                    .flattenSwitch(),
                )
                .flattenSwitch,
            )
            .flattenSwitch,
        pending = (_, _) => Signal.fromValue(PageLoadingIndicatorSkeleton.html),
      )
      .flattenSwitch

  html
}
