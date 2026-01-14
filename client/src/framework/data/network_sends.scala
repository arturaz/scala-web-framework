package framework.data

import scala.annotation.targetName
import cats.Functor
import cats.syntax.all.*
import framework.utils.AuthenticatedNetworkRequestFailure

/** A type alias for the network request that can fail with a network or authentication error. */
type SendRequestIO[AuthError, Response] = EitherT[IO, AuthenticatedNetworkRequestFailure[AuthError], Response]

/** The signal for sending the request. The when the outer [[Option]] becomes [[Some]] the button will be enabled. When
  * the user click the button the [[SyncIO]] will be executed. If it returns [[None]] nothing will happen, if it returns
  * [[Some]] the inner [[AppSendRequestIO]] will be executed.
  *
  * @tparam AuthError
  *   error if authentication fails, use [[Nothing]] if authentication is not required
  */
case class SendSignal[AuthError, Response](
  signal: Signal[Option[SyncIO[Option[SendRequestIO[AuthError, Response]]]]]
) {
  lazy val canSendSignal: Signal[Boolean] = signal.map(_.isDefined)

  def map[Response2](f: Response => Response2): SendSignal[AuthError, Response2] =
    SendSignal(signal.mapSome(_.map(_.map(_.map(f)))))

  def semiflatMap[Response2](f: Response => IO[Response2]): SendSignal[AuthError, Response2] =
    SendSignal(signal.mapSome(_.map(_.map(_.semiflatMap(f)))))
}
object SendSignal {

  /** Signal which does not ask for confirmation, but can fail auth and can be not available. */
  case class SignalMagnet[AuthError, Response](underlying: SignalMagnet.Underlying[AuthError, Response])
  object SignalMagnet extends SignalMagnetLowPriorityConversions {
    type Underlying[AuthError, Response] = Signal[Option[SendRequestIO[AuthError, Response]]]

    /** When the auth can fail and [[SendRequestIO]] is not always available. */
    given fromUnderlying[AuthError, Response]
      : Conversion[Underlying[AuthError, Response], SignalMagnet[AuthError, Response]] = apply

    /** When the auth can fail and [[SendRequestIO]] is always available. */
    given fromAlwaysAvailable[AuthError, Response]
      : Conversion[SendRequestIO[AuthError, Response], SignalMagnet[AuthError, Response]] = sendRequest =>
      apply(Signal.fromValue(Some(sendRequest)))

    /** When the auth cannot fail and is always available. */
    given fromAlwaysAvailableNeverFails[Response]: Conversion[IO[Response], SignalMagnet[Nothing, Response]] =
      sendRequest => apply(Signal.fromValue(Some(EitherT.liftF(sendRequest))))

    /** When the auth cannot fail and [[IO]] is not always available. */
    given fromIO[Response]: Conversion[Signal[Option[IO[Response]]], SignalMagnet[Nothing, Response]] =
      _.mapSome(sendRequest => EitherT.liftF(sendRequest))
  }
  trait SignalMagnetLowPriorityConversions extends SignalMagnetLowPriorityConversions2 {

    /** When we do not make a network request and the value may not be available. */
    given fromKnownValue[A]: Conversion[Signal[Option[A]], SignalMagnet[Nothing, A]] =
      _.mapSome(a => EitherT.pure[IO, AuthenticatedNetworkRequestFailure[Nothing]](a))
  }
  trait SignalMagnetLowPriorityConversions2 {

    /** When we do not make a network request and the value is always available. */
    given fromAlwaysAvailableKnownValue[A]: Conversion[Signal[A], SignalMagnet[Nothing, A]] =
      _.map(a => Some(EitherT.pure[IO, AuthenticatedNetworkRequestFailure[Nothing]](a)))
  }

  /** Signal which does not ask for confirmation. */
  def withoutConfirmation[AuthError, Response](
    magnet: SignalMagnet[AuthError, Response]
  ): SendSignal[AuthError, Response] =
    apply(magnet.underlying.mapSome(sendRequest => SyncIO.pure(Some(sendRequest))))

  /** Signal which possibly asks for confirmation. */
  def withMaybeConfirmation[AuthError, Response](
    question: MaybeSignal[Option[String]],
    magnet: SignalMagnet[AuthError, Response],
  ): SendSignal[AuthError, Response] =
    apply(
      magnet.underlying
        .combineWithFn(question.deunionizeSignal)((maybeSendRequest, maybeQuestion) =>
          maybeSendRequest.map((_, maybeQuestion))
        )
        .mapSome {
          case (sendRequest, None)           => SyncIO.pure(Some(sendRequest))
          case (sendRequest, Some(question)) => SyncIO { if (window.confirm(question)) Some(sendRequest) else None }
        }
    )

  /** Signal which asks for confirmation. */
  def withConfirmation[AuthError, Response](
    question: MaybeSignal[String],
    magnet: SignalMagnet[AuthError, Response],
  ): SendSignal[AuthError, Response] =
    apply(
      magnet.underlying
        .combineWithFn(question.deunionizeSignal)((maybeSendRequest, question) => maybeSendRequest.map((_, question)))
        .mapSome { case (sendRequest, question) =>
          SyncIO { if (window.confirm(question)) Some(sendRequest) else None }
        }
    )

  given functor[AuthError]: Functor[[Response] =>> SendSignal[AuthError, Response]] with {
    override def map[A, B](fa: SendSignal[AuthError, A])(f: A => B): SendSignal[AuthError, B] =
      fa.map(f)
  }
}
