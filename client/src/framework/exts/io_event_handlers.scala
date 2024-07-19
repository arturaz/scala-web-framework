package framework.exts

import com.raquo.laminar.keys.{EventProcessor, LockedEventKey}
import com.raquo.laminar.modifiers.{Binder, EventListener}
import com.raquo.laminar.nodes.ReactiveElement
import org.scalajs.dom.{Element, Event}

extension [Ev <: Event, V](eventProcessor: EventProcessor[Ev, V]) {

  /** Allows using [[IO]] actions as event listeners. */
  inline def --->(f: V => IO[Unit]): EventListener[Ev, V] = {
    eventProcessor --> { v =>
      val io = f(v)
      io.unsafeRunAndForget()
    }
  }
}

extension [Ev <: Event, In, Out](key: LockedEventKey[Ev, In, Out]) {

  /** Allows using [[IO]] actions as event listeners. */
  inline def --->(f: Out => IO[Unit]): Binder[ReactiveElement[Element]] = {
    key --> { v =>
      val io = f(v)
      io.unsafeRunAndForget()
    }
  }
}
