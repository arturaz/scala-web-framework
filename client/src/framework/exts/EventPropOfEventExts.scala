package framework.exts

import com.raquo.laminar.api.*
import com.raquo.laminar.keys.{EventProcessor, EventProp}
import org.scalajs.dom.Event

import scala.util.control.NonFatal

extension (eventProp: EventProp[Event]) {

  /** Tries to parse the value of the event as a [[BigDecimal]], returns [[None]] if it fails. */
  def mapToValueBigDecimal: EventProcessor[Event, Option[BigDecimal]] =
    eventProp.mapToValue.map {
      case "" => None
      case v =>
        try { Some(BigDecimal(v)) }
        catch { case NonFatal(_) => None }
    }

  /** Tries to parse the value of the event as a [[Double]], returns [[None]] if it fails. */
  def mapToValueDouble: EventProcessor[Event, Option[Double]] =
    eventProp.mapToValue.map {
      case "" => None
      case v  => v.toDoubleOption
    }

  /** Tries to parse the value of the event as a [[Int]], returns [[None]] if it fails. */
  def mapToValueInt: EventProcessor[Event, Option[Int]] =
    eventProp.mapToValue.map {
      case "" => None
      case v  => v.toIntOption
    }

  /** Tries to parse the value of the event as a [[Long]], returns [[None]] if it fails. */
  def mapToValueLong: EventProcessor[Event, Option[Long]] =
    eventProp.mapToValue.map {
      case "" => None
      case v  => v.toLongOption
    }

  /** Tries to parse the value of the event as a [[Short]], returns [[None]] if it fails. */
  def mapToValueShort: EventProcessor[Event, Option[Short]] =
    eventProp.mapToValue.map {
      case "" => None
      case v  => v.toShortOption
    }

  /** Tries to parse the value of the event as a [[Byte]], returns [[None]] if it fails. */
  def mapToValueByte: EventProcessor[Event, Option[Byte]] =
    eventProp.mapToValue.map {
      case "" => None
      case v  => v.toByteOption
    }
}
