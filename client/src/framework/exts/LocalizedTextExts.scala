package framework.exts

import com.raquo.airstream.core.Source
import com.raquo.laminar.inserters.DynamicInserter
import framework.data.MaybeSignal
import framework.localization.LocalizationSupport
import com.raquo.airstream.core.Source.SignalSource

extension (using l18n: LocalizationSupport)(text: l18n.LocalizedText) {

  /** Returns a signal that emits the localized text. */
  def textSignal(using locale: Signal[l18n.LocaleEnum]): Signal[String] =
    locale.map(l => text.text(using l))

  /** Returns a [[DynamicInserter]] that will insert the localized text and rerender it when the locale changes. */
  def textRx(using locale: Source[l18n.LocaleEnum]): DynamicInserter =
    L.child.text <-- locale.toObservable.map(l => text.text(using l))
}

extension [A](signal: Signal[A]) {

  /** Returns a signal that emits the localized text. */
  def textSignal(using l18n: LocalizationSupport)(f: A => l18n.LocalizedText)(using
    locale: Signal[l18n.LocaleEnum]
  ): Signal[String] =
    signal.combineWithFn(locale)((a, locale) => f(a).text(using locale))

  /** Returns a [[DynamicInserter]] that will insert the localized text and rerender it when the locale changes. */
  def textRx(using l18n: LocalizationSupport)(f: A => l18n.LocalizedText)(using
    locale: SignalSource[l18n.LocaleEnum]
  ): DynamicInserter =
    L.child.text <-- signal.combineWithFn(locale.toObservable)((a, locale) => f(a).text(using locale))
}
