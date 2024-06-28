package framework.exts

import framework.prelude.{executionContext, ioRuntime, logError}
import com.raquo.airstream.core.EventStream
import cats.effect.IO

extension [A](io: IO[A]) {

  /** Launches the [[IO]], emiting the event in the [[EventStream]] when it produces a result. */
  def toEventStream(): EventStream[A] = EventStream.fromFuture(io.unsafeToFuture())

  def unsafeRunAsyncOrHandleError(onValue: A => Unit): Unit = {
    io.unsafeRunAsync {
      case Right(value) => onValue(value)
      case Left(error)  => logError(error)
    }
  }
}
