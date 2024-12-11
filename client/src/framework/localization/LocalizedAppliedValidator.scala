package framework.localization

import yantl.Validator

/** Bundles everything together to get localized error messages. */
trait LocalizedAppliedValidator[A](val l18n: LocalizationSupport) {
  type TError

  def validator: Validator[A, TError]

  def localeSignal: Signal[l18n.LocaleEnum]

  def localizer: l18n.LocalizedTextOfValue[TError]

  /** The localized error messages. */
  def errorMessages(value: A): Signal[Vector[String]] =
    localeSignal.mapImplicit(validator.validate(value).map(localizer.text))
}
object LocalizedAppliedValidator {
  inline def apply[A](using validator: LocalizedAppliedValidator[A]): LocalizedAppliedValidator[A] = validator

  given instance[A, TError_](using
    l18n: LocalizationSupport,
    localeSignal: Signal[l18n.LocaleEnum],
    validator: Validator[A, TError_],
    localizer: l18n.LocalizedTextOfValue[TError_],
  ): LocalizedAppliedValidator[A] = {
    val _validator = validator
    val _localeSignal = localeSignal
    val _localizer = localizer

    new LocalizedAppliedValidator[A](l18n) { self =>
      type TError = TError_

      def validator: Validator[A, TError] = _validator

      def localeSignal = _localeSignal.asInstanceOf[Signal[self.l18n.LocaleEnum]]

      def localizer = _localizer.asInstanceOf[self.l18n.LocalizedTextOfValue[TError]]
    }
  }
}
