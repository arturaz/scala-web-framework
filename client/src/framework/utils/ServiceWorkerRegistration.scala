package framework.utils

import scala.concurrent.Future
import org.scalajs.dom.ServiceWorkerRegistration
import framework.exts.*
import cats.syntax.all.*

/** Utilities for working with service workers.
  *
  * Also see https://gist.github.com/Rich-Harris/fd6c3c73e6e707e312d7c5d7d0f3b2f9 for heap of useful information about
  * this monstrosity.
  */
object ServiceWorkerRegistration {

  /** Returns [[None]] if service workers are not supported. */
  def get: Future[Option[ServiceWorkerRegistration]] =
    window.navigator.serviceWorkerOption.map(_.ready.toFuture).sequence
}
