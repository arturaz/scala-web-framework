package framework.exts

import com.raquo.airstream.state.StrictSignal
import cats.effect.IO

extension [A](signal: StrictSignal[A]) {
  def nowIO: IO[A] = IO(signal.now())
}
