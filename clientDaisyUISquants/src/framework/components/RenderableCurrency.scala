package framework.components

import squants.market.{Currency, Money, MoneyContext}

import java.text.DecimalFormat
import scala.math.BigDecimal.RoundingMode

trait RenderableCurrency[-A] {
  def asString(value: A, currency: Currency): String
}
object RenderableCurrency {
  given bigDecimal: RenderableCurrency[BigDecimal] = (value, currency) => {
    val bigDecimal = value.setScale(currency.formatDecimals, RoundingMode.HALF_UP).bigDecimal
    // Render with separation, for example "1,000,000.34"
    val format = new DecimalFormat(show"#,##0.${"0" * currency.formatDecimals}")
    format.format(bigDecimal)
  }

  given money(using mc: MoneyContext): RenderableCurrency[Money] = (value, currency) => {
    val newValue = value.in(currency)
    bigDecimal.asString(newValue.amount, currency)
  }

  given newTypeWrapper[TWrapped, TUnderlying](using
    newtype: yantl.Newtype.WithType[TUnderlying, TWrapped],
    rc: RenderableCurrency[TUnderlying],
  ): RenderableCurrency[TWrapped] =
    (value, currency) => rc.asString(newtype.unwrap(value), currency)
}
