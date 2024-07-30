package framework.exts

import cats.effect.IO
import com.raquo.airstream.core.{Observer, Signal}
import com.raquo.airstream.ownership.ManualOwner
import framework.utils.SplitEnumSignal

import scala.deriving.Mirror

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

  /** @see [[SplitEnum.apply]] */
  inline def splitEnum(using mirror: Mirror.SumOf[A]): SplitEnumSignal.Splitter[A, mirror.MirroredElemTypes, Nothing] =
    SplitEnumSignal(signal)
}
