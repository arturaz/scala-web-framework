package framework.exts

import scala.concurrent.duration.*
import java.util.Locale
import java.util.concurrent.TimeUnit
import framework.prelude.{*, given}

extension (duration: Duration) {

  /** Selects most appropriate TimeUnit for given duration and formats it accordingly, with 4 digits precision * */
  def prettyFractional: String = prettyFractional(includeNanos = false)

  /** Selects most appropriate TimeUnit for given duration and formats it accordingly */
  def prettyFractional(includeNanos: Boolean, precision: Int = 4): String = {
    require(precision > 0, "precision must be > 0")

    duration match {
      case d: FiniteDuration =>
        val nanos = d.toNanos
        val unit = chooseUnit(nanos)
        val value = nanos.toDouble / TimeUnit.NANOSECONDS.convert(1, unit)

        s"%.${precision}g %s%s".formatLocal(
          Locale.ROOT,
          value,
          abbreviate(unit),
          if (includeNanos) s" ($nanos ns)" else "",
        )

      case Duration.MinusInf => s"-∞ (minus infinity)"
      case Duration.Inf      => s"∞ (infinity)"
      case _                 => "undefined"
    }
  }

  def chooseUnit(nanos: Long): TimeUnit = {
    val d = nanos.nanos

    if (d.toDays > 0) TimeUnit.DAYS
    else if (d.toHours > 0) TimeUnit.HOURS
    else if (d.toMinutes > 0) TimeUnit.MINUTES
    else if (d.toSeconds > 0) TimeUnit.SECONDS
    else if (d.toMillis > 0) TimeUnit.MILLISECONDS
    else if (d.toMicros > 0) TimeUnit.MICROSECONDS
    else TimeUnit.NANOSECONDS
  }

  def abbreviate(unit: TimeUnit): String = unit match {
    case TimeUnit.NANOSECONDS  => "ns"
    case TimeUnit.MICROSECONDS => "μs"
    case TimeUnit.MILLISECONDS => "ms"
    case TimeUnit.SECONDS      => "s"
    case TimeUnit.MINUTES      => "min"
    case TimeUnit.HOURS        => "h"
    case TimeUnit.DAYS         => "d"
  }
}
