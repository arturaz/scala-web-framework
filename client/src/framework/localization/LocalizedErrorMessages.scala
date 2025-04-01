package framework.localization

import framework.data.MaybeSignal
import cats.Contravariant
import yantl.Validate

/** Provides the (presumably localized) error messages for a given value. */
trait LocalizedErrorMessages[-A] { self =>

  /** The localized error messages. */
  def errorMessages(value: A): Signal[Vector[String]]

  /** Contramaps the value to a different type. */
  def contramap[B](fn: B => A): LocalizedErrorMessages[B] = new {
    def errorMessages(value: B): Signal[Vector[String]] =
      self.errorMessages(fn(value))
  }

  /** Allows you to either use this instance for the type [[B]] or custom error messages. */
  def contraemap[B](fn: B => Either[Signal[Vector[String]], A]): LocalizedErrorMessages[B] = new {
    def errorMessages(value: B): Signal[Vector[String]] =
      fn(value) match {
        case Left(errorMsgs) => errorMsgs
        case Right(value)    => self.errorMessages(value)
      }
  }
}

object LocalizedErrorMessages {
  def of[A](f: A => MaybeSignal[Vector[String]]): LocalizedErrorMessages[A] = new {
    def errorMessages(value: A): Signal[Vector[String]] = f(value).deunionizeSignal
  }

  given contravariant: Contravariant[LocalizedErrorMessages] with {

    override def contramap[A, B](fa: LocalizedErrorMessages[A])(f: B => A): LocalizedErrorMessages[B] = fa.contramap(f)

  }

  /** Summons an instance implicitly, collecting all parts via the `using` clause, delegating to
    * [[LocalizedAppliedValidate]].
    */
  given instance[A, TError_](using
    l18n: LocalizationSupport,
    localeSignal: Signal[l18n.LocaleEnum],
    validate: MaybeSignal[Validate[A, TError_]],
    localizer: l18n.LocalizedTextOfValue[TError_],
  ): LocalizedErrorMessages[A] = LocalizedAppliedValidate.instance
}
