package framework.prelude

import com.raquo.laminar.modifiers.{Modifier, RenderableText}
import framework.localization.LocalizationSupport
import org.scalajs.dom.{IntersectionObserver, IntersectionObserverEntry, IntersectionObserverInit}

import scala.util.chaining.*

import L.*

trait IntersectionObserverCallback[El <: Element] {
  def apply(observer: IntersectionObserver, entry: IntersectionObserverEntry, ctx: MountContext[El]): Unit
}

/** Returns the events that are emitted by the [[IntersectionObserver]]. */
def intersectionObserverEvents[El <: Element](
  callback: IntersectionObserverCallback[El],
  options: Option[IntersectionObserverInit] = None,
): Modifier[El] =
  onMountUnmountCallbackWithState(
    (ctx: MountContext[El]) => {
      val cb: scalajs.js.Function2[js.Array[IntersectionObserverEntry], IntersectionObserver, Unit] = {
        (entries, observer) =>
          val entry = entries.head
          callback(observer, entry, ctx)
      }
      val observer = options match {
        case None          => IntersectionObserver(cb)
        case Some(options) => IntersectionObserver(cb, options)
      }

      observer.observe(ctx.thisNode.ref)
      observer
    },
    (el, observer) => observer.unobserve(el.ref),
  )

/** Notifies you when the top of the element enters or exits the viewport. */
def observeTopVisibility[El <: L.Element](
  onEnter: IntersectionObserverCallback[El],
  onExit: IntersectionObserverCallback[El],
): Modifier[El] = {
  var visible = false

  intersectionObserverEvents(
    options = Some(
      js.Dynamic
        .literal()
        .asInstanceOf[IntersectionObserverInit]
        // This creates a 1px observation area at the top of the element
        .tap(_.rootMargin = "0px 0px -99% 0px")
    ),
    callback = { (obs, entry, ctx) =>
      // Get the position of the top relative to viewport
      val topPosition = entry.boundingClientRect.top
      log.debug("obs", obs, "entry", entry, "ctx", ctx, "topPosition", topPosition)

      if (topPosition >= 0) {
        if (!visible) {
          visible = true
          onEnter(obs, entry, ctx)
        }
      } else {
        if (visible) {
          visible = false
          onExit(obs, entry, ctx)
        }
      }
    },
  )
}

/** Notifies you when the top of the element enters or exits the viewport. */
def observeTopVisibility[El <: L.Element](sink: Sink[Boolean]): Modifier[El] = {
  val obs = sink.toObserver
  obs.onNext(false)
  observeTopVisibility(
    onEnter = (_, _, _) => obs.onNext(true),
    onExit = (_, _, _) => obs.onNext(false),
  )
}

/** Runs the callback when the element becomes visible. */
def onVisible[El <: L.Element](callback: IntersectionObserverCallback[El]): Modifier[El] =
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
  intersectionObserverEvents { (obs, entry, ctx) =>
    if (entry.isIntersecting) callback(obs, entry, ctx)
  }

/** Provides a [[RenderableText]] instance for [[LocalizedText]] given that you have [[LocalEnum]] in scope. */
given renderableTextForLocalizedText(using l18n: LocalizationSupport)(using
  l18n.LocaleEnum
): RenderableText[l18n.LocalizedText] =
  RenderableText(_.text)

given renderableTextForShow[A: Show]: RenderableText[A] = RenderableText(_.show)
