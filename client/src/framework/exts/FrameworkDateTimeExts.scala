package framework.exts

import scala.scalajs.js.Date
import framework.data.FrameworkDateTime

extension (dt: FrameworkDateTime) {

  /** Converts the date and time to a JavaScript [[Date]]. */
  def toJsDate: Date =
    new Date(dt.toInstant.getEpochSecond().toDouble * 1000)

  /** Renders the date and time in the client's local timezone. */
  def asClientLocalTimeString: String = {
    val date = dt.toJsDate
    show"${date.toLocaleDateString()} ${date.toLocaleTimeString()}"
  }
}
