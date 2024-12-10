package framework.data

import yantl.Newtype
import framework.utils.NewtypeString

/** A very basic email validator. */
object Email extends NewtypeString {
  type TError = NotAnEmail

  override val validators = IArray(Validator)

  /** Validates that it has a "x@y.z" format */
  def Validator: Newtype.Validator[String, NotAnEmail] = input => {
    input.indexOf('@') match {
      case -1 => Some(NotAnEmail(input))
      case etaAt =>
        input.lastIndexOf('.') match {
          case -1 => Some(NotAnEmail(input))
          case lastDotAt =>
            if (lastDotAt <= etaAt) Some(NotAnEmail(input))
            else None
        }
    }
  }

  case class NotAnEmail(email: String) derives CanEqual
}
type Email = Email.Type
