package framework.utils

import framework.data.MaybeSignal

enum PageTitleResult derives CanEqual {

  /** Use the default page title returned by the router. */
  case Default(renderInDocument: Boolean) extends PageTitleResult

  /** Use a custom page title. */
  case Custom(title: MaybeSignal[String], renderInDocument: Boolean = true) extends PageTitleResult

  /** If false then the page title should only be used in the <title> tag and not in the <body>. */
  def renderInDocument: Boolean
}
object PageTitleResult {
  val default: PageTitleResult = Default(renderInDocument = true)

  case class Result(pageTitle: String, renderInDocument: Boolean) {

    /** Returns the page title if [[renderInDocument]] is true. */
    def pageTitleForDocument: Option[String] = if (renderInDocument) Some(pageTitle) else None

    /** Returns the page title as a [[L.Node]] if [[renderInDocument]] is true.
      *
      * Example:
      * {{{
      * h1(
      *   child.maybe <-- pageTitleSignal.map(_.pageTitleNodeForDocument),
      *   pageTitleSignal --> (_.applyWindowTitle("My App"))
      * )
      * }}}
      */
    def pageTitleNodeForDocument: Option[L.Node] = pageTitleForDocument.map(L.textToTextNode)

    /** Returns the window title, which is the page title appended with the app name. */
    def windowTitle(appName: String): String = {
      if (pageTitle == appName) appName else show"$pageTitle - $appName"
    }

    /** Applies the [[windowTitle]] to the window.
      *
      * Example: {{{pageTitleSignal --> (_.applyWindowTitle("My App"))}}}
      */
    def applyWindowTitle(appName: String): Unit = {
      org.scalajs.dom.document.title = windowTitle(appName)
    }
  }

  extension (signal: Signal[PageTitleResult]) {

    /** @param defaultPageTitleSignal
      *   the signal to use when [[PageTitleResult]] is [[Default]]
      */
    def pageTitleSignal(defaultPageTitleSignal: Signal[String]) = {
      signal.flatMapSwitch {
        case PageTitleResult.Default(renderInDocument)       => defaultPageTitleSignal.map(Result(_, renderInDocument))
        case PageTitleResult.Custom(title, renderInDocument) => title.deunionizeSignal.map(Result(_, renderInDocument))
      }
    }
  }
}
