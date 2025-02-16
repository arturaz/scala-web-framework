package framework.data

import framework.utils.NetworkOrAuthError
import scala.annotation.targetName
import cats.Functor

/** A type alias for the network request that can fail with a network or authentication error. */
type SendRequestIO[AuthError, Response] = EitherT[IO, NetworkOrAuthError[AuthError], Response]

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

  /** Signal which does not ask for confirmation. */
  def withoutConfirmation[AuthError, Response](
    signal: Signal[Option[SendRequestIO[AuthError, Response]]]
  ): SendSignal[AuthError, Response] =
    apply(signal.mapSome(send => SyncIO.pure(Some(send))))

  /** Signal which is always available and does not ask for confirmation. */
  def withoutConfirmation[AuthError, Response](
    send: SendRequestIO[AuthError, Response]
  ): SendSignal[AuthError, Response] =
    apply(Signal.fromValue(Some(SyncIO.pure(Some(send)))))

  /** Signal which does not fail auth and does not ask for confirmation. */
  def withoutConfirmation[A](send: IO[A]): SendSignal[Nothing, A] =
    withoutConfirmation(EitherT.liftF(send))

  /** Signal which does not fail auth and does not ask for confirmation. */
  @targetName("withoutConfirmationSignalOptionWithoutAuth")
  def withoutConfirmation[A](signal: Signal[Option[IO[A]]]): SendSignal[Nothing, A] =
    withoutConfirmation(signal.mapSome(EitherT.liftF))

  /** Signal which does not fail auth and does not ask for confirmation. */
  @targetName("withoutConfirmationSignalOptionPureWithoutAuth")
  def withoutConfirmation[A](signal: Signal[Option[A]]): SendSignal[Nothing, A] =
    withoutConfirmation(signal.mapSome(IO.pure))

  /** Signal which asks for confirmation. */
  def withConfirmation[AuthError, Response](
    question: String,
    signal: Signal[Option[SendRequestIO[AuthError, Response]]],
  ): SendSignal[AuthError, Response] =
    apply(signal.mapSome(send => SyncIO { if (window.confirm(question)) Some(send) else None }))

  /** Signal which is always available and asks for confirmation. */
  def withConfirmation[AuthError, Response](
    question: String,
    send: SendRequestIO[AuthError, Response],
  ): SendSignal[AuthError, Response] =
    apply(Signal.fromValue(Some(SyncIO {
      if (window.confirm(question)) Some(send) else None
    })))

  given functor[AuthError]: Functor[[Response] =>> SendSignal[AuthError, Response]] with {
    override def map[A, B](fa: SendSignal[AuthError, A])(f: A => B): SendSignal[AuthError, B] =
      fa.map(f)
  }
}
