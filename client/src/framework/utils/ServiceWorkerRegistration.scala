package framework.utils

import scala.concurrent.Future
import org.scalajs.dom.ServiceWorkerRegistration
import framework.exts.*
import cats.syntax.all.*

object ServiceWorkerRegistration {

  /** Returns [[None]] if service workers are not supported. */
  def get: Future[Option[ServiceWorkerRegistration]] =
    window.navigator.serviceWorkerOption.map(_.ready.toFuture).sequence
}
