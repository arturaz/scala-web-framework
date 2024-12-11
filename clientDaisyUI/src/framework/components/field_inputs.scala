package framework.components

import framework.utils.UpdatableSignal
import L.*
import alleycats.Empty
import io.scalaland.chimney.Transformer
import framework.data.FrameworkDate
import framework.localization.LocalizationSupport
import framework.localization.LocalizedAppliedValidator

/** A text-based field. */
def TextLikeFieldInput[A: Empty: CanEqual1](
  signal: UpdatableSignal[A],
  submitting: Signal[Boolean],
  inputModifiers: Seq[HtmlMod] = Seq.empty,
)(using l18n: LocalizationSupport)(using
  PerformValidations,
  l18n.LocaleEnum,
  LocalizedAppliedValidator[A],
  Transformer[A, String],
  Transformer[String, A],
  TextKindFor[A],
  l18n.LocalizedTextOf[A],
) = {
  FormInput.textLikeWithLabelLocalized(
    signal,
    validation = PerformValidations.summon,
    inputModifiers = inputModifiers ++ Vector(disabled <-- submitting),
  )
}

/** A text-based optional field. */
def TextLikeOptionalFieldInput[A: Empty: CanEqual1](
  signal: UpdatableSignal[A],
  submitting: Signal[Boolean],
  inputModifiers: Seq[HtmlMod] = Seq.empty,
)(using l18n: LocalizationSupport)(using
  PerformValidations,
  l18n.LocaleEnum,
  LocalizedAppliedValidator[A],
  Transformer[A, String],
  Transformer[String, A],
  TextKindFor[A],
  l18n.LocalizedTextOf[A],
)(using optSignal: UpdatableSignal[Option[A]])(using FormInput.RemoveOptionalFieldButtonContent) = {
  FormInput.textLikeWithLabelLocalized(
    signal,
    validation = PerformValidations.summon,
    inputModifiers = inputModifiers ++ Vector(disabled <-- submitting),
    altLabel = Seq(FormInput.removeOptionalFieldButton(optSignal)),
  )
}

def DateLikeOptionalFieldInput[A: Empty: CanEqual1](
  signal: UpdatableSignal[A],
  submitting: Signal[Boolean],
)(using l18n: LocalizationSupport)(using
  PerformValidations,
  l18n.LocaleEnum,
  LocalizedAppliedValidator[A],
  Transformer[A, FrameworkDate],
  Transformer[FrameworkDate, A],
  l18n.LocalizedTextOf[A],
)(using optSignal: UpdatableSignal[Option[A]])(using FormInput.RemoveOptionalFieldButtonContent) = {
  FormInput.dateWithLabelLocalized(
    signal,
    validation = PerformValidations.summon,
    modInput = _.amend(disabled <-- submitting),
    renderPosition = RenderPosition.Sideways,
    altLabel = Seq(FormInput.removeOptionalFieldButton(optSignal)),
  )
}

/** The content in the card title that allows to remove that optional card. */
def OptionalFieldCardNextToTitle[A: Empty: CanEqual1](
  optSignal: UpdatableSignal[Option[A]]
)(using
  l18n: LocalizationSupport
)(using l18n.LocaleEnum, l18n.LocalizedTextOf[A], FormInput.RemoveOptionalFieldButtonContent) =
  Seq(div(cls := "grow"), FormInput.removeOptionalFieldButton(optSignal))
