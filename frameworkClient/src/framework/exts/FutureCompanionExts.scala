package framework.exts

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

extension (obj: Future.type) {

  /** Returns a [[Future]] that completes after the specified duration. */
  def delay(duration: FiniteDuration): Future[Unit] = {
    val promise = scala.concurrent.Promise[Unit]()
    scala.scalajs.js.timers.setTimeout(duration)(promise.success(()))
    promise.future
  }
}
