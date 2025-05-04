package framework.exts

import scala.concurrent.duration.*
import framework.utils.PrettyPrintDuration

extension (duration: Duration) {

  /** Pretty print a duration for human readability.
    *
    * @see
    *   [[PrettyPrintDuration.prettyPrint]]
    */
  def pretty(maxParts: Int = 2, maxGranularity: TimeUnit = SECONDS)(using
    PrettyPrintDuration.Strings
  ): String =
    PrettyPrintDuration.prettyPrint(duration, maxParts = maxParts, maxGranularity = maxGranularity)

  /** [[pretty]] with no limit on the number of parts. Used for debugging purposes. */
  def prettyForDebug(using
    strings: PrettyPrintDuration.Strings = PrettyPrintDuration.Strings.EnglishShortNoSpaces
  ): String =
    PrettyPrintDuration.prettyPrint(duration)
}
