package framework.data

import framework.utils.NewtypeString

/** A very basic email validator. */
object Email extends NewtypeString {
  override inline def validate(input: String): Boolean | String = {
    // Validate that it has a "x@y.z" format
    input.indexOf('@') match {
      case -1 => s"Invalid email: $input"
      case etaAt =>
        input.lastIndexOf('.') match {
          case -1 => s"Invalid email: $input"
          case lastDotAt =>
            if (lastDotAt <= etaAt) s"Invalid email: $input"
            else true
        }
    }
  }
}
type Email = Email.Type
