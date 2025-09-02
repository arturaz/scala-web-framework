package framework.utils

import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L
import framework.exts.*
import framework.data.MaybeSignal

/** Helper to dynamically compose the content of a page and the page title.
  *
  * @param content
  *   the content of the page
  * @param pageTitle
  *   the page title
  * @param externalModifiers
  *   modifiers to apply to the `content`s parent. These useful when we want to bind some event streams to something
  *   that is not the `content` itself, because that event stream then generates the `content`.
  */
case class PageRenderResult(
  content: MaybeSignal[L.Element],
  pageTitle: MaybeSignal[PageTitleResult] = PageTitleResult.default,
  externalModifiers: MaybeSignal[Seq[L.Modifier.Base]] = Seq.empty,
) {

  /** @see [[PageTitleResult.pageTitleSignal]] */
  def pageTitleSignal(defaultPageTitleSignal: Signal[String]): Signal[PageTitleResult.Result] =
    pageTitle.deunionizeSignal.pageTitleSignal(defaultPageTitleSignal)

  def withExternalModifiers(f: Seq[L.Modifier.Base] => Seq[L.Modifier.Base]): PageRenderResult =
    copy(externalModifiers = externalModifiers.deunionizeSignal.map(f))

  def mapContent(f: L.Element => MaybeSignal[L.Element]): PageRenderResult =
    copy(content = content.deunionizeSignal.flatMapSwitch(f(_).deunionizeSignal))
}
object PageRenderResult {
  given Conversion[L.Element, PageRenderResult] = fromElement(_)

  def fromElement(
    content: L.Element,
    pageTitle: MaybeSignal[PageTitleResult] = PageTitleResult.default,
  ): PageRenderResult = apply(content, pageTitle)

  extension (signal: Signal[PageRenderResult]) {

    /** Turns a [[Signal]] of [[PageRenderResult]] into a [[PageRenderResult]]. */
    def unsignal: PageRenderResult = {
      val content = signal.flatMapSwitch(_.content.deunionizeSignal)
      val pageTitle = signal.flatMapSwitch(_.pageTitle.deunionizeSignal)
      val externalModifiers = signal.flatMapSwitch(_.externalModifiers.deunionizeSignal)

      PageRenderResult(content, pageTitle, externalModifiers)
    }
  }
}
