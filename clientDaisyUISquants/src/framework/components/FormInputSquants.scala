package framework.components

import com.raquo.laminar.nodes.ReactiveHtmlElement
import framework.localization.LocalizationSupport
import framework.utils.{UpdatableSignal, ZoomedOwnerlessSignal}
import io.scalaland.chimney.Transformer
import squants.market.Currency
import squants.mass.{Mass, MassUnit}

import L.*
import framework.localization.LocalizedAppliedValidator

object FormInputSquants {
  def money[A](
    currencySignal: Signal[Option[Currency]],
    signal: ZoomedOwnerlessSignal[A],
    style: ComponentStyle = ComponentStyle.Standalone,
    inputModifiers: Modifier[Input]*
  )(using fromA: Transformer[A, BigDecimal], toA: Transformer[BigDecimal, A]) = {
    div(
      child.maybe <-- currencySignal.mapSome(_.symbol),
      FormInput.bigDecimal(
        signal,
        when(style.isTableCell)(cls := "w-16"),
        inputModifiers,
      ),
    )
  }

  def moneyWithLabelLocalized[A](
    currencySignal: Signal[Option[Currency]],
    signal: ZoomedOwnerlessSignal[A],
    validation: Option[LocalizedAppliedValidator[A]],
    altLabel: Seq[Modifier[Span]] = Seq.empty,
    inputModifiers: Modifier[Input]*
  )(using l18n: LocalizationSupport)(using
    lto: l18n.LocalizedTextOf[A],
    locale: l18n.LocaleEnum,
    fromA: Transformer[A, BigDecimal],
    toA: Transformer[BigDecimal, A],
  ) = {
    val label = lto.text

    div(
      cls := "mb-4",
      L.label(
        when(label.nonEmpty || altLabel.nonEmpty)(
          div(
            cls := "label",
            when(label.nonEmpty)(span(cls := "label-text", label)),
            when(altLabel.nonEmpty)(span(cls := "label-text-alt", altLabel)),
          )
        ),
        div(
          cls := "input input-bordered flex items-center gap-2",
          money(currencySignal, signal, inputModifiers = inputModifiers),
        ),
      ),
      FormInput.validationMessages(signal.signal, validation),
    )
  }

  // def money[A](
  //   signal: UpdatableSignal[A],
  //   modifiers: Modifier[ReactiveHtmlElement[HTMLInputElement]]*
  // )(using fromA: Transformer[A, Money], toA: Transformer[Money, A]) = {
  //   div(
  //     input(
  //       `type` := "number",
  //       modifiers,
  //       controlled(
  //         value <-- signal.signal.map(fromA.transform(_).amount.show),
  //         onInput.mapToValueBigDecimal --> { amount =>
  //           signal.update { current =>
  //             val newMoney = fromA.transform(current).currency(amount)
  //             toA.transform(newMoney)
  //           }
  //         },
  //       ),
  //     )
  //   )
  // }

  /** Creates a form input with a selector that allows you to change the unit.
    *
    * @note
    *   you want to use [[app.data.FormQuantity]] instead of using [[squants.Quantity]] types directly because we need
    *   the distinction between `5000kg` and `5t`.
    */
  def mass[A](
    signal: UpdatableSignal[A],
    availableUnits: NonEmptyVector[MassUnit],
    modifiers: Modifier[Input]*
  )(using fromA: Transformer[A, Mass], toA: Transformer[Mass, A])(using CanEqual1[MassUnit]) = {
    val massSignal = signal.bimap(fromA.transform)((_, mass) => toA.transform(mass))
    val initialUnit = massSignal.now().unit
    val realAvailableUnits =
      if (availableUnits.toVector.contains(initialUnit)) availableUnits else initialUnit +: availableUnits

    val currentUnit = massSignal.bimap(_.unit)((mass, unit) => mass in unit)

    div(
      input(
        `type` := "number",
        modifiers,
        controlled(
          value <-- massSignal.signal.map(_.value.show),
          onInput.mapToValueDouble.orElseEmpty --> { amount =>
            massSignal.update(_.unit(amount))
          },
        ),
      ),
      FormInput.select(
        currentUnit,
        realAvailableUnits.map(unit => (unit, unit.symbol)),
        style = ComponentStyle.TableCell,
      ),
    )
  }
}
