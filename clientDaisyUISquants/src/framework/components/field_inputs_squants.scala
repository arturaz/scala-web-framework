package framework.components

import alleycats.Empty
import framework.localization.LocalizationSupport
import framework.utils.UpdatableSignal
import io.scalaland.chimney.Transformer
import squants.market.Currency

import L.*
import framework.localization.LocalizedAppliedValidate

/** A money-based optional field. */
def MoneyLikeOptionalFieldInput[A: Empty: CanEqual1](
  signal: UpdatableSignal[A],
  submitting: Signal[Boolean],
  currency: Signal[Option[Currency]],
)(using l18n: LocalizationSupport)(using
  PerformValidations,
  l18n.LocaleEnum,
  LocalizedAppliedValidate[A],
  Transformer[A, BigDecimal],
  Transformer[BigDecimal, A],
  l18n.LocalizedTextOf[A],
)(using optSignal: UpdatableSignal[Option[A]])(using FormInput.RemoveOptionalFieldButtonContent) = {
  FormInputSquants.moneyWithLabelLocalized(
    currency,
    signal,
    validation = PerformValidations.whenEnabledSummonValue,
    inputModifiers = Vector(L.disabled <-- submitting),
    altLabel = Seq(FormInput.removeOptionalFieldButton(optSignal)),
  )
}
