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
      * child.maybe <-- pageTitleSignal.map(_.pageTitleNodeForDocument.map { node =>
      *   h1(cls := "text-2xl font-bold mb-4", node)
      * }),
      * pageTitleSignal --> (_.applyWindowTitle(L18n.AppName)),
      * }}}
      */
    def pageTitleNodeForDocument: Option[L.Node] = pageTitleForDocument.map(L.textToTextNode)

    /** Returns the window title, which is the page title appended with the app name.
      *
      * @param prefix
      *   a custom prefix which is not considered a part of the page title, for example this can be used to indicate a
      *   number of unread items.
      */
    def windowTitle(appName: String, prefix: Option[String] = None): String = {
      def withPrefix(str: String) = prefix match {
        case None         => str
        case Some(prefix) => s"$prefix$str"
      }

      withPrefix(if (pageTitle == appName) appName else show"$pageTitle - $appName")
    }

    /** Applies the [[windowTitle]] to the window.
      *
      * Example: {{{pageTitleSignal --> (_.applyWindowTitle("My App"))}}}
      */
    def applyWindowTitle(appName: String, prefix: Option[String] = None): Unit = {
      org.scalajs.dom.document.title = windowTitle(appName, prefix)
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
