package framework.prelude

import com.raquo.laminar.modifiers.Modifier
import org.scalajs.dom.{IntersectionObserver, IntersectionObserverEntry}
import com.raquo.laminar.modifiers.RenderableText
import framework.localization.LocalizationSupport

/** Runs the callback when the element becomes visible. */
def onVisible[A <: L.Element](callback: (A, IntersectionObserverEntry) => Unit) = Modifier[A] { elem =>
  // Taken from https://stackoverflow.com/a/66394121:
  //
  // function onVisible(element, callback) {
  //   new IntersectionObserver((entries, observer) => {
  //     entries.forEach(entry => {
  //       if(entry.intersectionRatio > 0) {
  //         callback(element);
  //         observer.disconnect();
  //       }
  //     });
  //   }).observe(element);
  //   if(!callback) return new Promise(r => callback=r);
  // }
  IntersectionObserver { (entries, observer) =>
    entries.find(_.isIntersecting).foreach(entry => callback(elem, entry))
  }.observe(elem.ref)
}

/** Provides a [[RenderableText]] instance for [[LocalizedText]] given that you have [[LocalEnum]] in scope. */
given renderableTextForLocalizedText[LocaleEnum](using
  LocaleEnum
): RenderableText[LocalizationSupport[LocaleEnum]#LocalizedText] =
  RenderableText(_.text)
