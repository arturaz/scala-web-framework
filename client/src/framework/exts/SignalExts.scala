package framework.exts

import cats.effect.IO
import com.raquo.airstream.core.{Observer, Signal}
import com.raquo.airstream.ownership.ManualOwner

import scala.deriving.Mirror
import alleycats.Empty

extension [A](signal: Signal[A]) {

  /** Subscribes to a [[Signal]] to receive the first value and immediately unsubscribes. */
  def toIO: IO[A] = {
    IO.async[A] { callback =>
      IO {
        val owner = new ManualOwner
        signal.addObserver(Observer(a => {
          owner.killSubscriptions()
          callback(Right(a))
        }))(owner)

        Some(IO(owner.killSubscriptions()))
      }
    }
  }

  /** Maps the value of the [[Signal]] passing the [[A]] as context. */
  def mapImplicit[B](f: A ?=> B): Signal[B] = signal.map(a => f(using a))
}

extension [A](signal: Signal[Option[A]]) {

  /** Splits the [[Signal]] by the [[Option]] cases, returning the [[Empty]] value if the [[Option]] is [[None]]. */
  def splitOptionOrEmpty[B](f: (A, Signal[A]) => B)(using empty: Empty[B]): Signal[B] =
    signal.splitOption(f, empty.empty)
}
