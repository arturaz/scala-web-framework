package framework.exts

import framework.prelude.{executionContext, ioRuntime, log}
import com.raquo.airstream.core.EventStream
import cats.effect.IO

implicit class IOExts[A](private val io: IO[A]) extends AnyVal {

  /** Launches the [[IO]], emiting the event in the [[EventStream]] when it produces a result. */
  def toEventStream(): EventStream[A] = EventStream.fromFuture(io.unsafeToFuture())

  def unsafeRunAsyncOrHandleError(onValue: A => Unit): Unit = {
    io.unsafeRunAsync {
      case Right(value) => onValue(value)
      case Left(error)  => log.error(error)
    }
  }
}
