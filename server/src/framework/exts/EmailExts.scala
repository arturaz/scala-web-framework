package framework.exts

import framework.data.Email
import pencilmail.data.Mailbox

extension (email: Email) {
  def asMailbox: Mailbox = Mailbox.unsafeFromString(email.unwrap)
}
