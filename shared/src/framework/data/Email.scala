package framework.data

import framework.exts.*
import framework.prelude.{*, given}
import framework.utils.NewtypeString
import yantl.{Newtype, ValidatorRule}

import scala.util.Try
import scala.util.matching.Regex

/** A very basic email validator. */
object Email extends NewtypeString with Newtype.ValidatedOf(IArray(Email.validatorRule)) {

  /** Validates that it has a "x@y.z" format */
  def validatorRule: ValidatorRule[String, NotAnEmail] = input => {
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

  /** Extend an `object` with this to get a new type of email that is validated.
    *
    * Example:
    * {{{
    *   object MyEmail extends Email.Validated(Set(Email.Validator.anyEmail))
    * }}}
    */
  abstract class Validated(val validEmails: Set[Validator])
      extends Newtype.ValidatedOf(yantl.Validator.of(Validator.anyOf(validEmails.toSeq*))) {

    given Show[Type] = _.unwrap.unwrap
    given CirceCodec[Type] = CirceCodec[Email].iemap(make.asString(_))(_.unwrap)
    given CanEqual1[Type] = CanEqual.derived

    def schemaFor: Schema[Type] = Email.schema.map(make(_).toOption)(_.unwrap)
  }

  case class Validator(regex: Regex) extends yantl.ValidatorRule[Email, ValidationError] {
    override def validate(email: Email): Option[ValidationError] =
      if (regex.matches(email.unwrap)) None else Some(ValidationError(email, NonEmptyVector.one(regex)))

    def isValid(s: Email): Boolean = regex.matches(s.unwrap)
  }
  object Validator {

    /** Allows any email. */
    def anyEmail: Validator = apply(""".*""".r)

    def fromString(s: String): Either[String, Validator] =
      Try(apply(Regex(s))).toEither.left.map(_.getMessage)

    given circeDecoder: CirceDecoder[Validator] =
      CirceDecoder.decodeString.emap(fromString)

    /** Validates that any of the given validations pass. */
    def anyOf(validEmails: Validator*): yantl.ValidatorRule[Email, ValidationError] =
      yantl.ValidatorRule.of { email =>
        val passes = validEmails.exists(_.isValid(email))

        if (passes) None
        else {
          val errors = validEmails.iterator.flatMap(_.validate(email)).flatMap(_.failed.toVector).toVector
          NonEmptyVector.fromVector(errors).map(failed => ValidationError(email, failed))
        }
      }
  }

  /** @param email
    *   the email that has been validated
    * @param failed
    *   the regexes that failed against the email
    */
  case class ValidationError(email: Email, failed: NonEmptyVector[Regex]) {
    override def toString(): String = "Invalid email format."
  }
}
type Email = Email.Type
