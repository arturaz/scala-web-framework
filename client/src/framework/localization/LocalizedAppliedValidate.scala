package framework.localization

import yantl.Validate

/** Bundles everything together to get localized error messages. */
trait LocalizedAppliedValidate[A](val l18n: LocalizationSupport) {
  type TError

  def validate: Validate[A, TError]

  def localeSignal: Signal[l18n.LocaleEnum]

  def localizer: l18n.LocalizedTextOfValue[TError]

  /** The localized error messages. */
  def errorMessages(value: A): Signal[Vector[String]] =
    localeSignal.mapImplicit(validate.validate(value).map(localizer.text))
}
object LocalizedAppliedValidate {
  inline def apply[A](using validator: LocalizedAppliedValidate[A]): LocalizedAppliedValidate[A] = validator

  given instance[A, TError_](using
    l18n: LocalizationSupport,
    localeSignal: Signal[l18n.LocaleEnum],
    validate: Validate[A, TError_],
    localizer: l18n.LocalizedTextOfValue[TError_],
  ): LocalizedAppliedValidate[A] = {
    val _validate = validate
    val _localeSignal = localeSignal
    val _localizer = localizer

    new LocalizedAppliedValidate[A](l18n) { self =>
      type TError = TError_

      def validate: Validate[A, TError] = _validate

      def localeSignal = _localeSignal.asInstanceOf[Signal[self.l18n.LocaleEnum]]

      def localizer = _localizer.asInstanceOf[self.l18n.LocalizedTextOfValue[TError]]
    }
  }
}
