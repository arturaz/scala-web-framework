package framework.exts

import com.raquo.laminar.keys.EventProcessor
import com.raquo.laminar.modifiers.EventListener
import org.scalajs.dom
import framework.prelude.ioRuntime
import cats.effect.IO

extension [Ev <: dom.Event, V](eventProcessor: EventProcessor[Ev, V]) {

  /** Allows using [[IO]] actions as event listeners. */
  inline def --->(f: V => IO[Unit]): EventListener[Ev, V] = {
    eventProcessor --> { v =>
      val io = f(v)
      io.unsafeRunAndForget()
    }
  }
}
