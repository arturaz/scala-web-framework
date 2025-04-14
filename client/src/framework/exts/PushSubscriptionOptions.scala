package framework.exts

import org.scalajs.dom.PushSubscriptionOptions
import framework.utils.PushNotifications

extension (opts: PushSubscriptionOptions) {
  def asString: String =
    show"userVisibleOnly=${opts.userVisibleOnly.toOption}, " +
      show"key=${opts.applicationServerKey.toOption.map(PushNotifications.PublicKey.fromJs)}"
}
