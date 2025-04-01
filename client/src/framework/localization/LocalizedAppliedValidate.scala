package framework.localization

import framework.data.MaybeSignal
import framework.localization.LocalizationSupport.LocalizedTextOfValue
import yantl.Validate

/** Bundles everything together to get localized error messages.
  *
  * This trait is not transformable due to use of path-dependent types, you should use
  * [[LocalizedAppliedValidate.create]] instead with a composed [[Validate]] or use [[LocalizedErrorMessages]] instead.
  */
trait LocalizedAppliedValidate[-A] extends LocalizedErrorMessages[A] { self =>
  type TError
  type LocaleEnum

  def validate: Signal[Validate[A, TError]]

  def localeSignal: Signal[LocaleEnum]

  def localizer: LocalizationSupport.LocalizedTextOfValue[TError, LocaleEnum]

  override def errorMessages(value: A): Signal[Vector[String]] =
    localeSignal.combineWithFn(validate)((locale, validate) =>
      validate.validate(value).map(localizer.localizedText(_).text(using locale))
    )
}
object LocalizedAppliedValidate {
  type Aux[A, TError_, LocaleEnum_] = LocalizedAppliedValidate[A] {
    type TError = TError_; type LocaleEnum = LocaleEnum_
  }

  /** Allows writing `LocalizedAppliedValidate[MyType]` instead of `summon[LocalizedAppliedValidate[MyType]]`. */
  inline def apply[A](using validator: LocalizedAppliedValidate[A]): LocalizedAppliedValidate[A] = validator

  /** Creates an instance explicitly from the provided [[Validate]] instance. */
  def create[A, Error](using l18n: LocalizationSupport, localeSignal: Signal[l18n.LocaleEnum])(
    validate: MaybeSignal[Validate[A, Error]]
  )(using localizer: l18n.LocalizedTextOfValue[Error]): LocalizedAppliedValidate.Aux[A, Error, l18n.LocaleEnum] = {
    instance[A, Error](using validate = validate)
  }

  /** Summons an instance implicitly, collecting all parts via the `using` clause. */
  given instance[A, TError_](using
    l18n: LocalizationSupport,
    localeSignal: Signal[l18n.LocaleEnum],
    validate: MaybeSignal[Validate[A, TError_]],
    localizer: l18n.LocalizedTextOfValue[TError_],
  ): LocalizedAppliedValidate.Aux[A, TError_, l18n.LocaleEnum] = {
    val _validate = validate
    val _localeSignal = localeSignal
    val _localizer = localizer

    new LocalizedAppliedValidate[A] { self =>
      type TError = TError_
      type LocaleEnum = l18n.LocaleEnum

      val validate: Signal[Validate[A, TError]] = _validate.deunionizeSignal

      def localeSignal = _localeSignal

      def localizer = _localizer
    }
  }
}
