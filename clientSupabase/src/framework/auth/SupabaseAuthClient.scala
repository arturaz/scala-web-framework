package framework.auth

import cats.Show
import framework.data.Email
import framework.sourcecode.DefinedAt
import framework.utils.JSLogger
import retry.syntax.*
import retry.{HandlerDecision, RetryPolicies}
import typings.supabaseAuthJs.anon.{DataMessageId, DataSessionUser, DataUser, ErrorAuthError, MessageId, User}
import typings.supabaseAuthJs.distModuleLibErrorsMod.{isAuthRetryableFetchError, AuthError}
import typings.supabaseAuthJs.distModuleLibTypesMod.{
  _AuthChangeEvent,
  EmailOtpType,
  Session,
  SignInWithPasswordlessCredentials,
  VerifyOtpParams,
}
import typings.supabaseSupabaseJs.distModuleLibSupabaseAuthClientMod.SupabaseAuthClient as JSSupabaseAuthClient

import scala.scalajs.js.JSON

import concurrent.duration.*

/** Wrapper around insane supabase APIs. */
class SupabaseAuthClient(
  private val client: JSSupabaseAuthClient,
  val log: JSLogger,
) {
  def autoRefreshToken: Boolean = client.autoRefreshToken

  lazy val onAuthStateChange: EventStream[SupabaseAuthClient.AuthStateChange] = {
    val log = this.log.scoped("onAuthStateChange")

    EventStream.fromCustomSourceWithSubscription(
      start = (fireValue: SupabaseAuthClient.AuthStateChange => Unit, fireError, getStartIndex, getIsStarted) => {
        client.onAuthStateChange { (jsEvt, jsSession) =>
          SupabaseAuthClient.AuthChangeEvent.from(jsEvt) match {
            case None =>
              log.error("unknown event: ", jsEvt)
            case Some(evt) =>
              val maybeSession = jsSession.jsValueOrNullToOption
              fireValue(SupabaseAuthClient.AuthStateChange(evt, maybeSession))
          }
        }
      },
      stop = (_, subscription) => {
        subscription.data.subscription.unsubscribe()
      },
    )
  }

  val retryPolicy = RetryPolicies
    .constantDelay[IO](50.millis)
    .join(RetryPolicies.limitRetries(10))
    .followedBy(RetryPolicies.limitRetriesByDelay(200.millis, RetryPolicies.exponentialBackoff[IO](10.millis)))

  def signInWithOtp(email: Email)(using DefinedAt): IO[Either[ErrorAuthError, MessageId]] = {
    val log = this.log.scoped("signInWithOtp")

    def doTry(tryIndex: Int) =
      IO(log(show"Sending OTP code to '$email' (try index: $tryIndex)")) *>
        IO.fromPromise(IO(client.signInWithOtp(SignInWithPasswordlessCredentials.EmailOptions(email.unwrap))))
          .flatTap(response => IO(log(show"OTP code-send response: ", response)))
          .map { r =>
            r.matchDynamic(_.error)
              .on(null)((v: DataMessageId) => Right(v.data))
              .performOrElse((v: ErrorAuthError) => Left(v))
          }

    doTry(0)
      .retryingOnFailures(
        retryPolicy,
        {
          case (Left(err), details) if isAuthRetryableFetchError(err.error) =>
            IO {
              log.info(show"failed, retrying (${details.pprintWithoutColors})", err.error)
              HandlerDecision.Adapt(doTry(details.retriesSoFar + 1))
            }
          case (Left(err), details) =>
            IO {
              log.error(show"failed (${details.pprintWithoutColors}):", err)
              HandlerDecision.Stop
            }
          case (Right(messageId), details) => {
            IO {
              log.info(show"succeeded (${details.pprintWithoutColors})", messageId)
              HandlerDecision.Stop
            }
          }
        },
      )
      .map(_.merge)
  }

  def verifyOtp(email: Email, otp: String)(using DefinedAt): IO[Either[AuthError, User]] = {
    val log = this.log.scoped("verifyOtp")

    def doTry(tryIndex: Int) =
      IO(log(show"Verifying OTP code '$otp' for '$email' (try index: $tryIndex)")) *>
        IO.fromPromise(
          IO(client.verifyOtp(VerifyOtpParams.VerifyEmailOtpParams(email.unwrap, otp, EmailOtpType.email)))
        ).flatTap(response => IO(log("OTP code-verify response: ", response)))
          .map { r =>
            if (r.asInstanceOf[js.Dynamic].error == null) Right(r.asInstanceOf[DataUser].data)
            else Left(r.asInstanceOf[DataSessionUser].error)
          }

    doTry(0)
      .retryingOnFailures(
        retryPolicy,
        {
          case (Left(err), details) if isAuthRetryableFetchError(err) =>
            IO.pure(HandlerDecision.Adapt(doTry(details.retriesSoFar + 1)))
          case (_, _) => IO.pure(HandlerDecision.Stop)
        },
      )
      .map(_.merge)
  }

  def signOut: IO[Either[AuthError, Unit]] =
    IO.fromPromise(IO(client.signOut())).map(_.error.jsValueOrNullToOption.toLeft(()))
}
object SupabaseAuthClient {
  enum AuthChangeEvent derives CanEqual {

    /** @see [[typings.supabaseAuthJs.supabaseAuthJsStrings.INITIAL_SESSION]] */
    case INITIAL_SESSION

    /** @see [[typings.supabaseAuthJs.supabaseAuthJsStrings.PASSWORD_RECOVERY]] */
    case PASSWORD_RECOVERY

    /** @see [[typings.supabaseAuthJs.supabaseAuthJsStrings.SIGNED_IN]] */
    case SIGNED_IN

    /** @see [[typings.supabaseAuthJs.supabaseAuthJsStrings.SIGNED_OUT]] */
    case SIGNED_OUT

    /** @see [[typings.supabaseAuthJs.supabaseAuthJsStrings.TOKEN_REFRESHED]] */
    case TOKEN_REFRESHED

    /** @see [[typings.supabaseAuthJs.supabaseAuthJsStrings.USER_UPDATED]] */
    case USER_UPDATED

    /** @see [[typings.supabaseAuthJs.distModuleLibTypesMod.AuthChangeEventMFA]] */
    case AuthChangeEventMFA
  }
  object AuthChangeEvent {
    given show: Show[AuthChangeEvent] = Show.fromToString

    def from(jsEvent: typings.supabaseAuthJs.distModuleLibTypesMod.AuthChangeEvent): Option[AuthChangeEvent] = {
      given CanEqual[
        typings.supabaseAuthJs.distModuleLibTypesMod.AuthChangeEvent,
        typings.supabaseAuthJs.supabaseAuthJsStrings.MFA_CHALLENGE_VERIFIED,
      ] = CanEqual.derived

      given CanEqual[
        typings.supabaseAuthJs.distModuleLibTypesMod.AuthChangeEvent,
        _AuthChangeEvent,
      ] = CanEqual.derived

      if (jsEvent == typings.supabaseAuthJs.supabaseAuthJsStrings.MFA_CHALLENGE_VERIFIED) Some(AuthChangeEventMFA)
      else if (jsEvent == typings.supabaseAuthJs.supabaseAuthJsStrings.INITIAL_SESSION) Some(INITIAL_SESSION)
      else if (jsEvent == typings.supabaseAuthJs.supabaseAuthJsStrings.PASSWORD_RECOVERY) Some(PASSWORD_RECOVERY)
      else if (jsEvent == typings.supabaseAuthJs.supabaseAuthJsStrings.SIGNED_IN) Some(SIGNED_IN)
      else if (jsEvent == typings.supabaseAuthJs.supabaseAuthJsStrings.SIGNED_OUT) Some(SIGNED_OUT)
      else if (jsEvent == typings.supabaseAuthJs.supabaseAuthJsStrings.TOKEN_REFRESHED) Some(TOKEN_REFRESHED)
      else if (jsEvent == typings.supabaseAuthJs.supabaseAuthJsStrings.USER_UPDATED) Some(USER_UPDATED)
      else None
    }
  }

  /** Emitted by [[ClientSideAuthClient.onAuthStateChange]] when the auth state changes. */
  case class AuthStateChange(event: AuthChangeEvent, maybeSession: Option[Session]) {
    override def toString(): String =
      show"AuthStateChange(event=$event, session=${maybeSession.map(JSON.stringify(_, space = 2))})"
  }
}
