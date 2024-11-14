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
}
