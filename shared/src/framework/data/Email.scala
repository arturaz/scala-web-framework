package framework.data

import framework.exts.*
import framework.prelude.{*, given}
import framework.utils.NewtypeString
import yantl.{Newtype, ValidatorRule}

import scala.util.Try
import scala.util.matching.Regex

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

  def schema: Schema[Type] =
    Schema.schemaForString.map(make(_).toOption)(_.unwrap).description("Email address")

  /** Mix this into an `object` to get a new type of email that is validated.
    *
    * Example:
    * {{{
    *   object MyEmail extends Email.Validated {
    *     override def ValidEmails: Set[Email.Validator] = Set(Email.Validator.anyEmail)
    *   }
    * }}}
    */
  trait Validated {
    object Validator {
      opaque type Type = Regex

      def apply(r: Regex): Type = r

      /** Allows any email. */
      def anyEmail: Type = """.*""".r

      def fromString(s: String): Either[String, Type] =
        Try(Regex(s)).toEither.left.map(_.getMessage)

      given circeDecoder: CirceDecoder[Type] =
        CirceDecoder.decodeString.emap(fromString)

      extension (v: Type) {
        def asRegex: Regex = v

        def isValid(s: Email): Boolean = v.matches(s.unwrap)
      }
    }
    type Validator = Validator.Type

    opaque type Type = Email

    def apply(
      email: Email,
      valid: Set[Validator] = ValidEmails,
    ): Either[String, Type] = {
      import Validator.isValid

      if (valid.exists(_.isValid(email))) Right(email)
      else Left(s"Invalid email: $email")
    }

    extension (email: Type) {
      def unwrap: Email = email
    }

    /** Emails that are valid. */
    def ValidEmails: Set[Validator]

    given Show[Type] = _.unwrap
    given CirceCodec[Type] = CirceCodec[Email].iemap(apply(_))(_.unwrap)

    def schemaFor: Schema[Type] = Email.schema.map(apply(_).toOption)(_.unwrap)
  }

}
type Email = Email.Type
