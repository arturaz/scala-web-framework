package framework.utils

import framework.prelude.{given_CanEqual1_Duration, given_CanEqual1_TimeUnit}

import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.concurrent.duration.*

/** Pretty-prints durations.
  *
  * Original version from https://gist.github.com/tg44/a8257da5831a43465beda4c613647375
  *
  * License: MIT (https://opensource.org/license/mit)
  */
object PrettyPrintDuration {

  private val timeUnitList: List[TimeUnit] =
    DAYS ::
      HOURS ::
      MINUTES ::
      SECONDS ::
      MILLISECONDS ::
      MICROSECONDS ::
      NANOSECONDS ::
      Nil

  @tailrec
  private def prettyRec(
    accumulator: List[FiniteDuration],
    remainingUnits: List[TimeUnit],
    remainingDuration: FiniteDuration,
    isPast: Boolean,
    maxParts: Option[Int],
  )(using strs: Strings): String = {
    remainingUnits match {
      case timeUnit :: remainingUnits if maxParts.isEmpty || maxParts.exists(accumulator.length < _) =>
        if (remainingDuration > Duration(1, timeUnit)) {
          val x = Duration(remainingDuration.toUnit(timeUnit).toLong, timeUnit)
          prettyRec(x :: accumulator, remainingUnits, remainingDuration - x, isPast, maxParts)
        } else {
          prettyRec(accumulator, remainingUnits, remainingDuration, isPast, maxParts)
        }
      case _ =>
        val result = accumulator.reverseIterator.map(strs.format).mkString(" ")
        if (isPast) strs.formatNegativeDuration(result) else result
    }
  }

  /** Formats the duration.
    *
    * @param duration
    *   the duration to format
    * @param maxParts
    *   the maximum number of parts (a part is a unit with a value, for example "5 seconds") to include in the output
    * @param maxGranularity
    *   the maximum granularity to include in the output. For example, `TimeUnit.SECONDS` will only include seconds and
    *   larger units, so "9d 3h 4m 1s 20ms 40μs 100ns" will be formatted as "9d 3h 4m 1s".
    * @param strs
    *   the [[Strings]] to use for formatting
    * @return
    *   the formatted duration
    */
  def prettyPrint(duration: Duration, maxParts: Int = Int.MaxValue, maxGranularity: TimeUnit = TimeUnit.NANOSECONDS)(
    using strs: Strings
  ): String = {
    def maxPartsAsOption = if (maxParts == Int.MaxValue) None else Some(maxParts)

    val timeUnitList =
      if (maxGranularity == TimeUnit.NANOSECONDS) this.timeUnitList
      else this.timeUnitList.takeWhile(_ != maxGranularity) ::: List(maxGranularity)

    duration match {
      case Duration.Zero => strs.zero
      case f: FiniteDuration if f < Duration.Zero =>
        prettyRec(Nil, timeUnitList, f * -1, isPast = true, maxPartsAsOption)
      case f: FiniteDuration => prettyRec(Nil, timeUnitList, f, isPast = false, maxPartsAsOption)
      case Duration.Inf      => strs.infinite
      case Duration.MinusInf => strs.minusInfinite
      case _                 => strs.unknown
    }
  }

  /** Pretty-prints using the default English implementation. */
  def prettyPrintEn(duration: Duration, maxParts: Int = Int.MaxValue): String =
    prettyPrint(duration, maxParts)(using Strings.English)

  /** Returns the texts used for formatting durations. */
  trait Strings {

    /** String for [[Duration.Zero]] */
    def zero: String

    /** Post-processes the result of [[prettyPrint]] for negative durations.
      *
      * For example, you can turn `-5.seconds` into "-5 s" or "5 s ago".
      */
    def formatNegativeDuration(prettyPrinted: String): String

    /** Formats the finite duration. */
    def format(duration: FiniteDuration): String

    /** String for [[Duration.Inf]] */
    def infinite: String

    /** String for [[Duration.MinusInf]] */
    def minusInfinite: String

    /** String for unknown durations. */
    def unknown: String
  }
  object Strings {

    /** Default implementation of [[Strings]] in English (3 days, 5 minutes). */
    object English extends Strings {
      override def formatNegativeDuration(prettyPrinted: String): String = s"$prettyPrinted ago"
      override def zero: String = "now"
      override def infinite: String = "infinity"
      override def minusInfinite: String = "negative infinity"
      override def unknown: String = "undefined"

      override def format(duration: FiniteDuration): String = duration.toString
    }

    /** Implementation of [[Strings]] in English with short units. */
    trait EnglishShort(useSpaces: Boolean) extends Strings {
      override def formatNegativeDuration(prettyPrinted: String): String = s"$prettyPrinted ago"
      override def zero: String = "now"
      override def infinite: String = "∞"
      override def minusInfinite: String = "-∞"
      override def unknown: String = "undefined"

      override def format(duration: FiniteDuration): String = {
        val l = duration.length
        val s = if (useSpaces) " " else ""

        duration.unit match {
          case TimeUnit.NANOSECONDS  => s"$l${s}ns"
          case TimeUnit.MICROSECONDS => s"$l${s}μs"
          case TimeUnit.MILLISECONDS => s"$l${s}ms"
          case TimeUnit.SECONDS      => s"$l${s}s"
          case TimeUnit.MINUTES      => s"$l${s}m"
          case TimeUnit.HOURS        => s"$l${s}h"
          case TimeUnit.DAYS         => s"$l${s}d"
        }
      }
    }

    /** Implementation of [[Strings]] in English with short units (3 d, 5 m). */
    object EnglishShort extends EnglishShort(useSpaces = true)

    /** Implementation of [[Strings]] in English with short units (3d, 5m). */
    object EnglishShortNoSpaces extends EnglishShort(useSpaces = false)
  }
}
