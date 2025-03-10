package framework.localization

import yantl.Validate

/** Bundles everything together to get localized error messages. */
trait LocalizedAppliedValidate[-A] {
  type TError
  type LocaleEnum

  def validate: Validate[A, TError]

  def localeSignal: Signal[LocaleEnum]

  def localizer: LocalizationSupport.LocalizedTextOfValue[TError, LocaleEnum]

  /** The localized error messages. */
  def errorMessages(value: A): Signal[Vector[String]] =
    localeSignal.mapImplicit(validate.validate(value).map(localizer.localizedText(_).text))
}
object LocalizedAppliedValidate {
  inline def apply[A](using validator: LocalizedAppliedValidate[A]): LocalizedAppliedValidate[A] = validator

  def create[A](using l18n: LocalizationSupport, localeSignal: Signal[l18n.LocaleEnum])(
    validate: Validate[A, l18n.LocalizedText]
  ): LocalizedAppliedValidate[A] = {
    val _validate = validate
    val _localeSignal = localeSignal

    new LocalizedAppliedValidate[A] { self =>
      type TError = l18n.LocalizedText
      type LocaleEnum = l18n.LocaleEnum

      def validate: Validate[A, TError] = _validate

      def localeSignal = _localeSignal

      lazy val localizer = l18n.LocalizedTextOfValue.of((localizedText, locale) => localizedText.text(using locale))
    }
  }

  given instance[A, TError_](using
    l18n: LocalizationSupport,
    localeSignal: Signal[l18n.LocaleEnum],
    validate: Validate[A, TError_],
    localizer: l18n.LocalizedTextOfValue[TError_],
  ): LocalizedAppliedValidate[A] = {
    val _validate = validate
    val _localeSignal = localeSignal
    val _localizer = localizer

    new LocalizedAppliedValidate[A] { self =>
      type TError = TError_
      type LocaleEnum = l18n.LocaleEnum

      def validate: Validate[A, TError] = _validate

      def localeSignal = _localeSignal

      def localizer = _localizer
    }
  }
}
