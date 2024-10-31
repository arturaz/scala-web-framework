package framework.exts

import com.raquo.airstream.core.Source
import com.raquo.laminar.inserters.DynamicInserter
import framework.localization.LocalizationSupport

extension [LocaleEnum](text: LocalizationSupport[LocaleEnum]#LocalizedText) {

  /** Returns a [[DynamicInserter]] that will insert the localized text and rerender it when the locale changes. */
  def textRx(using locale: Source[LocaleEnum]): DynamicInserter =
    L.child.text <-- locale.toObservable.map(l => text.text(using l))
}
