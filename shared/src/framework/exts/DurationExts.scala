package framework.exts

import scala.concurrent.duration.*
import framework.utils.PrettyPrintDuration

extension (duration: Duration) {

  /** @see [[PrettyPrintDuration.prettyPrint]] */
  def pretty(maxParts: Int = 2)(using PrettyPrintDuration.Strings): String =
    PrettyPrintDuration.prettyPrint(duration, maxParts = maxParts)

  /** [[pretty]] with no limit on the number of parts. */
  def prettyUnbounded(using PrettyPrintDuration.Strings): String =
    PrettyPrintDuration.prettyPrint(duration)
}
