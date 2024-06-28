package framework.exts

import cats.data.EitherT
import cats.effect.IO
import cats.syntax.option.*
import com.raquo.airstream.core.Signal
import framework.data.{AuthLoadingStatus, PublicLoadingStatus}
import framework.prelude.{*, given}
import framework.utils.NetworkError

import scala.concurrent.Future
import framework.utils.NetworkOrAuthError

extension (obj: Signal.type) {

  /** Creates a [[Signal]] from a [[EitherT]] of [[Future]]. */
  def fromFutureEitherT[A](
    future: => EitherT[Future, NetworkError, A]
  ): Signal[PublicLoadingStatus[A]] =
    fromFutureReturningOption(future.map(_.some))

  /** [[fromFutureEitherT]] for [[IO]]. */
  def fromIO[A](io: EitherT[IO, NetworkError, A]): Signal[PublicLoadingStatus[A]] =
    fromIOReturningOption(io.map(_.some))

  /** Creates a [[Signal]] from a [[Future]] that returns an [[Option]] where `None` is treated as "not found".
    */
  def fromFutureReturningOption[A](
    future: => EitherT[Future, NetworkError, Option[A]]
  ): Signal[PublicLoadingStatus[A]] =
    Signal.fromFuture(future.value).map {
      case None                        => PublicLoadingStatus.Loading
      case Some(Left(_: NetworkError)) => PublicLoadingStatus.NetworkError
      case Some(Right(None))           => PublicLoadingStatus.NotFound
      case Some(Right(Some(value)))    => PublicLoadingStatus.Loaded(value)
    }

  /** [[fromFutureReturningOption]] for [[IO]]. */
  def fromIOReturningOption[A](io: EitherT[IO, NetworkError, Option[A]]): Signal[PublicLoadingStatus[A]] =
    fromFutureReturningOption(io.mapK(ioToFutureFunctionK))

  /** Creates a [[Signal]] from a [[Future]] that performs authentication. */
  def fromAuthRequestFuture[A, AuthError](
    future: => EitherT[Future, NetworkOrAuthError[AuthError], A]
  ): Signal[AuthLoadingStatus[A]] =
    fromAuthRequestFutureReturningOption(future.map(_.some))

  /** [[fromAuthRequestFuture]] for [[IO]]. */
  def fromAuthRequestIO[A, AuthError](io: EitherT[IO, NetworkOrAuthError[AuthError], A]): Signal[AuthLoadingStatus[A]] =
    fromAuthRequestReturningOptionIO(io.map(_.some))

  /** Creates a [[Signal]] from a [[Future]] that performs authentication where `None` is treated as "not found".. */
  def fromAuthRequestFutureReturningOption[A, AuthError](
    future: => EitherT[Future, NetworkOrAuthError[AuthError], Option[A]]
  ): Signal[AuthLoadingStatus[A]] = {
    Signal.fromFuture(future.value).map {
      case None                                           => AuthLoadingStatus.Loading
      case Some(Left(NetworkOrAuthError.NetworkError(_))) => AuthLoadingStatus.NetworkError
      case Some(Left(NetworkOrAuthError.AuthError(_)))    => AuthLoadingStatus.Unauthenticated
      case Some(Right(None))                              => AuthLoadingStatus.NotFound
      case Some(Right(Some(value)))                       => AuthLoadingStatus.Loaded(value)
    }
  }

  /** [[fromAuthRequestFutureReturningOption]] for [[IO]]. */
  def fromAuthRequestReturningOptionIO[A, AuthError](
    io: EitherT[IO, NetworkOrAuthError[AuthError], Option[A]]
  ): Signal[AuthLoadingStatus[A]] =
    fromAuthRequestFutureReturningOption(io.mapK(ioToFutureFunctionK))
}
