package framework.exts

import com.raquo.laminar.keys.EventProcessor
import org.scalajs.dom.Event
import alleycats.Empty

extension [Ev <: Event, A](processor: EventProcessor[Ev, Option[A]]) {

  /** Removes the [[Option]] by returning the [[Empty]] value if the [[Option]] is [[None]]. */
  def orElseEmpty(using e: Empty[A]): EventProcessor[Ev, A] = processor.map(_.getOrElse(e.empty))
}
