package framework.data

import squants.space.{Length, LengthUnit}
import framework.prelude.{*, given}
import sttp.tapir.Schema
import sttp.tapir.SchemaType.SNumber
import framework.prelude.*
import framework.exts.*
import cats.syntax.all.*

/** Specifies that the [[Length]] value is in the specified [[LengthUnit]].
  *
  * Useful in serialization.
  *
  * Example:
  * {{{
  * case class RangeBased(range: LengthIn[InternationalMiles.type])
  * }}}
  */
case class LengthIn[U <: LengthUnit] private (length: Length) {

  /** Converts the length to the specified unit. */
  def in(unit: U): Length =
    length.in(unit)

  /** Returns the value of the length in the accompanying unit. */
  def value(using unit: ValueOf[U]): Double =
    length.in(unit.value).value

  /** Returns the value of the length in the specified unit. */
  def valueIn(unit: U): Double =
    length.in(unit).value

  /** Converts the length to the specified unit. */
  def to[U1 <: LengthUnit](unit: U1): LengthIn[U1] =
    LengthIn(length.in(unit))

  override def toString(): String = length.toString
}
object LengthIn {
  given [U <: LengthUnit]: Conversion[LengthIn[U], Length] = _.length
  given [U <: LengthUnit](using ValueOf[U]): Conversion[Length, LengthIn[U]] = apply

  given show[U <: LengthUnit]: Show[LengthIn[U]] = Show.fromToString

  given schema[U <: LengthUnit](using unit: ValueOf[U]): Schema[LengthIn[U]] =
    Schema(SNumber()).description(s"Length in ${unit.value} (${unit.value.symbol})")

  given circeCodec[U <: LengthUnit](using unit: ValueOf[U]): CirceCodec[LengthIn[U]] =
    CirceCodec.fromUsing[Double].imap(v => apply(unit.value(v)))(_.valueIn(unit.value))

  def fromValue[U <: LengthUnit](value: Double)(using unit: ValueOf[U]): LengthIn[U] =
    apply(unit.value(value))

  def fromLength[U <: LengthUnit](length: Length)(using unit: ValueOf[U]): LengthIn[U] =
    apply(length.in(unit.value))
}
