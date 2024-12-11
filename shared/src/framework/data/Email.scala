package framework.data

import yantl.Newtype
import framework.utils.NewtypeString
import yantl.ValidatorRule

/** A very basic email validator. */
object Email extends NewtypeString with Newtype.ValidatedOf(IArray(Email.Validator)) {

  /** Validates that it has a "x@y.z" format */
  def Validator: ValidatorRule[String, NotAnEmail] = input => {
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
