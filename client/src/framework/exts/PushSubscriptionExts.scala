package framework.exts

import framework.data.WebPushSubscription
import org.scalajs.dom.{PushSubscription, PushSubscriptionJSON}
import sttp.model.Uri

extension (jsSubscription: PushSubscription) {
  def asScala: Either[String, WebPushSubscription] =
    jsSubscription.toJSON().asScala
}

extension (json: PushSubscriptionJSON) {
  def asScala: Either[String, WebPushSubscription] = for {
    endpoint <- Uri.parse(json.endpoint)
    auth <- json.keys.get("auth").toRight("Cannot find 'auth' key")
    p256dh <- json.keys.get("p256dh").toRight("Cannot find 'p256dh' key")
  } yield WebPushSubscription(endpoint, WebPushSubscription.Keys(auth = auth, p256dh = p256dh))
}
