package framework.data

import framework.exts.*
import framework.prelude.{*, given}
import framework.utils.NewtypeString
import yantl.{Newtype, Validate, Validator, ValidatorRule}

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
    * object MyEmail extends Email.Validated {
    *   override def validEmails = Set(MyEmail.RegexValidator.anyEmail)
    * }
    * }}}
    */
  abstract class Validated extends Newtype {
    type TUnderlying = Email
    type TError = ValidationError

    def validEmails: Set[RegexValidator]

    // Lazy because otherwise we have a cyclic dependency.
    lazy val validator: Validator[TUnderlying, TError] = yantl.Validator.of(RegexValidator.anyOf(validEmails.toSeq*))

    override def validate: Validate[TUnderlying, TError] = validator

    given Show[Type] = v => Email.unwrap(v.unwrap)
    given CirceCodec[Type] = CirceCodec[Email].iemap(make.asString(_))(_.unwrap)
    given CanEqual1[Type] = CanEqual.derived

    def schemaFor: Schema[Type] = Email.schema.map(make(_).toOption)(_.unwrap)

    case class RegexValidator(regex: Regex) extends yantl.ValidatorRule[Email, ValidationError] {
      override def validate(email: Email): Option[ValidationError] =
        if (regex.matches(email.unwrap)) None else Some(ValidationError(email, NonEmptyVector.one(regex)))

      def isValid(s: Email): Boolean = regex.matches(s.unwrap)
    }
    object RegexValidator {
      given Conversion[RegexValidator, Regex] = _.regex

      /** Allows any email. */
      def anyEmail: RegexValidator = apply(""".*""".r)

      def fromString(s: String): Either[String, RegexValidator] =
        Try(apply(Regex(s))).toEither.left.map(_.getMessage)

      given circeDecoder: CirceDecoder[RegexValidator] =
        CirceDecoder.decodeString.emap(fromString)

      /** Validates that any of the given validations pass. */
      def anyOf(validEmails: RegexValidator*): yantl.ValidatorRule[Email, ValidationError] =
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
}
type Email = Email.Type
