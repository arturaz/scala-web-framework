package framework.components

import com.raquo.laminar.modifiers.RenderableText
import framework.data.MaybeSignal
import io.scalaland.chimney.Transformer
import squants.market.{Currency, Money, MoneyContext}
import squants.mass.{Mass, MassUnit}

import java.text.DecimalFormat
import scala.annotation.targetName

import L.*

object FormRenderSquants {
  // Render with separation, for example "1,000,000.34"
  val massFormat = new DecimalFormat("#,##0.0")

  def textWithCurrency[A: RenderableCurrency: RenderableText](
    signal: Signal[A],
    currency: Signal[Option[Currency]],
  ): Signal[String] =
    signal.combineWithFn(currency)(textWithCurrency)

  def textWithCurrency[A: RenderableCurrency: RenderableText](
    amount: A,
    currency: Option[Currency],
  ): String = (amount, currency) match {
    case (amount, None)           => summon[RenderableText[A]].asString(amount)
    case (amount, Some(currency)) => textWithCurrency(amount, currency)
  }

  def textWithCurrency[A](
    amount: A,
    currency: Currency,
  )(using rc: RenderableCurrency[A]): String = show"${currency.symbol}${rc.asString(amount, currency)}"

  def moneyText[A](value: A)(using toMoney: Transformer[A, Money]): String = {
    val money = toMoney.transform(value)
    textWithCurrency(money.amount, money.currency)
  }

  @targetName("textWithCurrencyFn")
  def textWithCurrency[A: RenderableCurrency](
    signal: Signal[Currency => A],
    currency: Signal[Option[Currency]],
  )(using mc: MoneyContext): Signal[String] =
    signal.combineWithFn(currency) { (amountFn, maybeCurrency) =>
      val currency = maybeCurrency.getOrElse(mc.defaultCurrency)
      val amount = amountFn(currency)
      textWithCurrency(amount, currency)
    }

  /** Renders a number of the mass with a selector that allows you to change the unit.
    *
    * @param availableUnits
    *   the units that will be shown in the selector
    * @param initialUnit
    *   the unit that will be selected by default. If this unit is not in the available units, it will be added as the
    *   first unit.
    */
  def textWithMassSelector(
    signal: MaybeSignal[Mass],
    availableUnits: NonEmptyVector[MassUnit],
    initialUnit: MassUnit,
    roundingScale: Int = 4,
  )(using CanEqual1[MassUnit]): L.Span = {
    val realAvailableUnits =
      if (availableUnits.toVector.contains(initialUnit)) availableUnits else initialUnit +: availableUnits

    val currentUnit = Var(initialUnit)

    span(
      // Prevent breaking the number and the selector to separate lines.
      cls := "text-nowrap",
      span(
        cls := "mr-2",
        child.text <-- signal.deunionizeSignal.combineWithFn(currentUnit.signal) { (mass, unit) =>
          val value = mass.in(unit).rounded(roundingScale).value
          massFormat.format(value)
        },
      ),
      FormInput.select(
        currentUnit,
        realAvailableUnits.map(unit => (unit, unit.symbol)),
        style = ComponentStyle.TableCell,
      ),
    )
  }

  def mass[A](
    a: A,
    roundingScale: Int = 4,
  )(using toMass: Transformer[A, Mass]): String = {
    val mass = toMass.transform(a).rounded(roundingScale)
    show"${massFormat.format(mass.value)} ${mass.unit.symbol}"
  }

  def massSignal[A](
    signal: Signal[A],
    roundingScale: Int = 4,
  )(using Transformer[A, Mass]): Span =
    span(child.text <-- signal.map(mass(_, roundingScale)))
}
