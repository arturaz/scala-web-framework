package framework.exts

import cats.data.EitherT
import cats.effect.IO
import cats.syntax.option.*
import com.raquo.airstream.core.Signal
import framework.data.{AuthLoadingStatus, PublicLoadingStatus}
import framework.prelude.{*, given}
import framework.utils.{AuthenticatedNetworkRequestFailure, NetworkOrAuthError, NetworkRequestFailure}

import scala.concurrent.Future
import scala.concurrent.duration.*
import framework.facades.AbortError

extension (obj: Signal.type) {

  /** Creates a [[Signal]] from a [[EitherT]] of [[Future]]. */
  def fromFutureEitherT[A](
    future: => EitherT[Future, NetworkRequestFailure, A]
  ): Signal[PublicLoadingStatus[A]] =
    fromFutureReturningOption(future.map(_.some))

  /** [[fromFutureEitherT]] for [[IO]]. */
  def fromIO[A](io: EitherT[IO, NetworkRequestFailure, A]): Signal[PublicLoadingStatus[A]] =
    fromIOReturningOption(io.map(_.some))

  /** Creates a [[Signal]] from a [[Future]] that returns an [[Option]] where `None` is treated as "not found".
    */
  def fromFutureReturningOption[A](
    future: => EitherT[Future, NetworkRequestFailure, Option[A]]
  ): Signal[PublicLoadingStatus[A]] =
    Signal.fromFuture(future.value).map {
      case None | Some(Left(NetworkRequestFailure.Aborted))  => PublicLoadingStatus.Loading
      case Some(Left(NetworkRequestFailure.NetworkError(_))) => PublicLoadingStatus.NetworkError
      case Some(Right(None))                                 => PublicLoadingStatus.NotFound
      case Some(Right(Some(value)))                          => PublicLoadingStatus.Loaded(value)
    }

  /** [[fromFutureReturningOption]] for [[IO]]. */
  def fromIOReturningOption[A](io: EitherT[IO, NetworkRequestFailure, Option[A]]): Signal[PublicLoadingStatus[A]] =
    fromFutureReturningOption(io.mapK(ioToFutureFunctionK))

  /** Creates a [[Signal]] from a [[Future]] that performs authentication. */
  def fromAuthRequestFuture[A, AuthError](
    future: => EitherT[Future, AuthenticatedNetworkRequestFailure[AuthError], A]
  ): Signal[AuthLoadingStatus[A]] =
    fromAuthRequestFutureReturningOption(future.map(_.some))

  /** [[fromAuthRequestFuture]] for [[IO]]. */
  def fromAuthRequestIO[A, AuthError](
    io: EitherT[IO, AuthenticatedNetworkRequestFailure[AuthError], A]
  ): Signal[AuthLoadingStatus[A]] =
    fromAuthRequestReturningOptionIO(io.map(_.some))

  /** Creates a [[Signal]] from a [[Future]] that performs authentication where `None` is treated as "not found".. */
  def fromAuthRequestFutureReturningOption[A, AuthError](
    future: => EitherT[Future, AuthenticatedNetworkRequestFailure[AuthError], Option[A]]
  ): Signal[AuthLoadingStatus[A]] = {
    Signal.fromFuture(future.value).map {
      case None | Some(Left(AuthenticatedNetworkRequestFailure.Aborted)) => AuthLoadingStatus.Loading
      case Some(Left(AuthenticatedNetworkRequestFailure.NetworkOrAuthError(NetworkOrAuthError.NetworkError(_)))) =>
        AuthLoadingStatus.NetworkError
      case Some(Left(AuthenticatedNetworkRequestFailure.NetworkOrAuthError(NetworkOrAuthError.AuthError(_)))) =>
        AuthLoadingStatus.Unauthenticated
      case Some(Right(None))        => AuthLoadingStatus.NotFound
      case Some(Right(Some(value))) => AuthLoadingStatus.Loaded(value)
    }
  }

  /** [[fromAuthRequestFutureReturningOption]] for [[IO]]. */
  def fromAuthRequestReturningOptionIO[A, AuthError](
    io: EitherT[IO, AuthenticatedNetworkRequestFailure[AuthError], Option[A]]
  ): Signal[AuthLoadingStatus[A]] =
    fromAuthRequestFutureReturningOption(io.mapK(ioToFutureFunctionK))

  /** Creates a [[Signal]] that emits the value of `f` every `period` milliseconds.
    *
    * @note
    *   You usually want to run `.distinct` on top of it.
    */
  def fromPolling[A](period: FiniteDuration)(f: => A): Signal[A] =
    EventStream.periodic(period.toMillis.toInt).mapToSignal(f)

  /** Creates a [[Signal]] that starts with the `initial` value and then runs Future `f` every `period` milliseconds to
    * update the value in the signal.
    *
    * While the future is resolving old value is kept in the signal.
    *
    * @note
    *   You usually want to run `.distinct` on top of it.
    */
  def fromPollingFuture[A](period: FiniteDuration, initial: => A)(f: => Future[A]): Signal[A] =
    EventStream.periodic(period.toMillis.toInt).flatMapSwitch(_ => EventStream.fromFuture(f)).toSignal(initial)

  def fromPollingFuture[A](period: FiniteDuration)(f: => Future[A]): Signal[Option[A]] =
    fromPollingFuture[Option[A]](period, None)(f.map(Some(_)))
}
