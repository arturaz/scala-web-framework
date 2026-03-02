package framework.exts

import cats.effect.IO
import cats.effect.kernel.{Fiber, Outcome}

extension (obj: IO.type) {

  /** Races two `IO[Option[A]]` computations and returns the first `Some` value.
    *
    * Returns `None` only if both sides complete with `None`.
    */
  def raceSome[A](ioa: IO[Option[A]], iob: IO[Option[A]]): IO[Option[A]] = {
    def resolveCompleted(outcome: Outcome[IO, Throwable, Option[A]]): IO[Option[A]] = outcome match {
      case Outcome.Succeeded(effect) => effect
      case Outcome.Errored(error)    => IO.raiseError(error)
      case Outcome.Canceled()        => IO.pure(None)
    }

    def resolveFirst(
      first: Outcome[IO, Throwable, Option[A]],
      other: Fiber[IO, Throwable, Option[A]],
    ): IO[Option[A]] = first match {
      case Outcome.Succeeded(effect) =>
        effect.flatMap {
          case some @ Some(_) =>
            other.cancel.as(some)
          case None =>
            other.join.flatMap(resolveCompleted)
        }
      case Outcome.Errored(error) =>
        other.cancel *> IO.raiseError(error)
      case Outcome.Canceled() =>
        other.join.flatMap(resolveCompleted)
    }

    IO.racePair(ioa, iob).flatMap {
      case Left((outcomeA, fiberB)) =>
        resolveFirst(outcomeA, fiberB)
      case Right((fiberA, outcomeB)) =>
        resolveFirst(outcomeB, fiberA)
    }
  }
}
