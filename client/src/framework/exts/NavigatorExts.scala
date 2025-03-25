package framework.exts

import org.scalajs.dom.{Navigator, ServiceWorkerContainer}

extension (navigator: Navigator) {

  /** Returns [[None]] if service workers are not supported. */
  def serviceWorkerOption: Option[ServiceWorkerContainer] =
    if (js.isUndefined(navigator.serviceWorker)) None else Option(navigator.serviceWorker)
}
