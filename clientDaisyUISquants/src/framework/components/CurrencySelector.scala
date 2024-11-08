package framework.components

import framework.utils.UpdatableSignal
import squants.market.{Currency, MoneyContext}
import framework.localization.LocalizationSupport

trait CurrencySelectorContent {
  def beforeSelect: Seq[L.Modifier.Base]
}

def CurrencySelector(
  signal: UpdatableSignal[Option[Currency]]
)(using
  mc: MoneyContext,
  l18n: LocalizationSupport,
  locale: l18n.LocaleEnum,
  lto: l18n.LocalizedTextOf[Currency],
  content: CurrencySelectorContent,
)(using CanEqual1[Currency]) = {
  FormInput.selectWithLabel(
    lto.text,
    signal,
    mc.currenciesAsSelectOptions,
    beforeSelect = content.beforeSelect,
  )
}
