package framework.data

import sttp.model.Uri
import framework.prelude.{*, given}

/** WebPush subscription as obtained from the client via
  * https://developer.mozilla.org/en-US/docs/Web/API/PushSubscription/toJSON
  */
case class WebPushSubscription(endpoint: Uri, keys: WebPushSubscription.Keys) derives CanEqual, CirceCodec {
  override def toString: String = s"WebPushSubscription(endpoint=$endpoint, $keys)"
}
object WebPushSubscription {
  case class Keys(auth: String, p256dh: String) derives CanEqual, CirceCodec {
    override def toString: String = s"Keys(auth=\"$auth\", p256dh=\"$p256dh\")"
  }
}
