package framework.exts

import com.raquo.airstream.core.AirstreamError
import com.raquo.airstream.ownership.{Owner, Subscription}

extension (obj: AirstreamError.type) {
  def registerUnhandledErrorCallbackSafe(callback: Throwable => Unit, owner: Owner): Subscription = {
    obj.registerUnhandledErrorCallback(callback)
    Subscription(owner, () => obj.unregisterUnhandledErrorCallback(callback))
  }
}
