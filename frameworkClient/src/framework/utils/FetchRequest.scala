package framework.utils

import alleycats.Empty
import cats.Functor
import cats.data.EitherT
import com.raquo.airstream.core.Signal
import com.raquo.airstream.split.Splittable
import com.raquo.airstream.state.Var
import framework.data.{AuthLoadingStatus, LoadingStatus, PublicLoadingStatus}
import framework.sourcecode.DefinedAt
import framework.utils.NetworkError
import cats.syntax.functor.*

import scala.annotation.targetName

/** A request that can be restarted after a network error.
  *
  * Useful for view-like components, use [[ModificationRequestTracker]] for data submission.
  */
class FetchRequest[Input, TNetworkError, TResultWrapper[+_], TLoadingStatus[+X] <: LoadingStatus[X], Result](
  createIO: Input => EitherT[IO, TNetworkError, TResultWrapper[Result]],
  ioToSignal: EitherT[IO, TNetworkError, TResultWrapper[Result]] => Signal[TLoadingStatus[Result]],
)(using Functor[TLoadingStatus], Empty[TLoadingStatus[Nothing]])
    extends FetchRequest.WithoutParams {
  private case class LastStartData(
    input: Input,
    signal: Signal[TLoadingStatus[FetchRequest.WithInput[Input, Result]]],
  )

  private val current: Var[Option[LastStartData]] = Var(None)

  val signal: Signal[TLoadingStatus[FetchRequest.WithInput[Input, Result]]] = current.signal
    .flatMapSwitch {
      case None       => Signal.fromValue(empty)
      case Some(data) => data.signal
    }

  val isLoading: Signal[Boolean] = signal.map(_.isLoading)

  def startWith(input: Input): Signal[TLoadingStatus[FetchRequest.WithInput[Input, Result]]] = {
    val io = createIO(input)
    val signal = ioToSignal(io).map(_.map(FetchRequest.WithInput(input, _)))
    val data = LastStartData(input, signal)
    current.set(Some(data))
    this.signal
  }

  def restart()(implicit definedAt: DefinedAt): Unit = {
    current.now() match {
      case None       => logError("Can't restart without input")
      case Some(data) => val _ = startWith(data.input)
    }
  }
}
object FetchRequest {

  /** The interface for [[FetchRequest]] that does not depend on type parameters. */
  trait WithoutParams {
    def isLoading: Signal[Boolean]

    def restart()(implicit definedAt: DefinedAt): Unit
  }

  case class WithInput[+Input, +A](input: Input, fetchedData: A) {
    def mapFetchedData[B](f: A => B): WithInput[Input, B] = WithInput(input, f(fetchedData))
  }

  @targetName("public")
  def apply[Input, Result](
    createIO: Input => EitherT[IO, NetworkError, Option[Result]]
  ): FetchRequest[Input, NetworkError, Option, PublicLoadingStatus, Result] = {
    new FetchRequest(createIO, Signal.fromIOReturningOption)
  }

  @targetName("publicInfallible")
  def apply[Input, Result](
    createIO: Input => EitherT[IO, NetworkError, Result]
  ): FetchRequest[Input, NetworkError, Id, PublicLoadingStatus, Result] = {
    new FetchRequest(createIO, Signal.fromIO)
  }

  @targetName("authenticatedInfallible")
  def apply[Input, AuthError, Result](
    createIO: Input => EitherT[IO, NetworkOrAuthError[AuthError], Result]
  ): FetchRequest[Input, NetworkOrAuthError[AuthError], Id, AuthLoadingStatus, Result] = {
    new FetchRequest(createIO, Signal.fromAuthRequestIO)
  }

  @targetName("authenticated")
  def apply[Input, AuthError, Result](
    createIO: Input => EitherT[IO, NetworkOrAuthError[AuthError], Option[Result]]
  ): FetchRequest[Input, NetworkOrAuthError[AuthError], Option, AuthLoadingStatus, Result] = {
    new FetchRequest(createIO, Signal.fromAuthRequestReturningOptionIO)
  }
}
