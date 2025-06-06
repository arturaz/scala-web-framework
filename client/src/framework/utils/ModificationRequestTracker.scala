package framework.utils

import cats.data.EitherT
import cats.effect.kernel.Outcome
import com.raquo.airstream.core.Signal
import com.raquo.airstream.state.{StrictSignal, Var}
import framework.utils.NetworkOrAuthError

/** Tracks the status of an ongoing request.
  *
  * This is used to show the user that an action is being performed and to cancel it if the user clicks the cancel
  * button.
  *
  * Most useful for data submission. For view-like actions, use [[FetchRequest]].
  */
class ModificationRequestTracker(status: Var[ModificationRequestTracker.Status]) {
  def signal: StrictSignal[ModificationRequestTracker.Status] = status.signal

  /** Is true when the request is being submitted. */
  val submitting: Signal[Boolean] = signal.map {
    case ModificationRequestTracker.Status.Standby    => false
    case ModificationRequestTracker.Status.EnRoute(_) => true
  }

  /** Returns [[Some]] with a cancellation function when the request is [[ModificationRequestTracker.Status.EnRoute]].
    */
  val canCancel: Signal[Option[() => Unit]] = signal.map {
    case ModificationRequestTracker.Status.Standby         => None
    case ModificationRequestTracker.Status.EnRoute(cancel) => Some(() => cancel.unsafeRunAndForget())
  }
  val canCancelBool: Signal[Boolean] = canCancel.map(_.isDefined)

  /** Launches a request. */
  def launch[A, Err](request: IO[Either[Err, A]]): IO[ModificationRequestTracker.Result[Err, A]] =
    launch(EitherT(request))

  /** Launches a request. */
  def launch[A, Err](request: EitherT[IO, Err, A]): IO[ModificationRequestTracker.Result[Err, A]] = {
    for {
      fiber <- (for {
        _ <- IO(status.now()).flatMap {
          case ModificationRequestTracker.Status.Standby         => IO.unit
          case ModificationRequestTracker.Status.EnRoute(cancel) => cancel
        }
        fiber <- request.value.start
        enRoute = ModificationRequestTracker.Status.EnRoute(fiber.cancel)
        _ <- IO(status.set(enRoute))
      } yield fiber).uncancelable
      outcome <- (fiber.join <* IO(status.set(ModificationRequestTracker.Status.Standby))).uncancelable
      result <- outcome match {
        case Outcome.Succeeded(fa) =>
          fa.map {
            case Left(err) => ModificationRequestTracker.Result.Error(err)
            case Right(a)  => ModificationRequestTracker.Result.Finished(a)
          }
        case Outcome.Errored(e) => IO.raiseError(e)
        case Outcome.Canceled() => IO.pure(ModificationRequestTracker.Result.Cancelled)
      }
    } yield result
  }
}
object ModificationRequestTracker {
  def apply(): ModificationRequestTracker = new ModificationRequestTracker(Var(Status.Standby))

  sealed trait Result[+Err, +A] derives CanEqual
  object Result {
    case object Cancelled extends Result[Nothing, Nothing]
    case class Error[+Err](err: Err) extends Result[Err, Nothing]
    case class Finished[+A](response: A) extends Result[Nothing, A]
  }

  sealed trait Status derives CanEqual
  object Status {

    /** The request is not being sent. */
    case object Standby extends Status

    /** The request is sent and we can cancel it. */
    case class EnRoute(cancel: IO[Unit]) extends Status
  }
}
