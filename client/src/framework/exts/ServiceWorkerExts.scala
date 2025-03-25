package framework.exts

import org.scalajs.dom.{MessageChannel, ServiceWorker}

import scala.concurrent.{Future, Promise}

extension (sw: ServiceWorker) {

  /** Posts a message to the service worker and waits to get a response back. */
  def postMessageAndWait(message: js.Any): Future[Any] = {
    val promise = Promise[Any]()
    val messageChannel = MessageChannel()
    messageChannel.port1.onmessage = evt => promise.success(evt.data)
    sw.postMessage(message, js.Array(messageChannel.port2))
    promise.future
  }
}
