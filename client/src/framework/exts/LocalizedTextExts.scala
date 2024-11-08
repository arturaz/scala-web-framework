package framework.exts

import com.raquo.airstream.core.Source
import com.raquo.laminar.inserters.DynamicInserter
import framework.localization.LocalizationSupport
import framework.data.MaybeSignal

extension (using l18n: LocalizationSupport)(text: l18n.LocalizedText) {
  def textSignal(using locale: Signal[l18n.LocaleEnum]): Signal[String] =
    locale.map(l => text.text(using l))

  /** Returns a [[DynamicInserter]] that will insert the localized text and rerender it when the locale changes. */
  def textRx(using locale: Source[l18n.LocaleEnum]): DynamicInserter =
    L.child.text <-- locale.toObservable.map(l => text.text(using l))
}
