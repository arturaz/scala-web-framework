package framework.utils

import cats.data.NonEmptyVector
import cats.syntax.show.*
import sttp.tapir.Schema

/** Indicates that the enum has conversions from/to [[String]]. Similar to `enumeratum` library, but less boilerplatey
  * and plays nice with Scala 3 `enum`s.
  */
trait NamedEnum[A] {

  /** All values of the enum. */
  def values: NonEmptyVector[A]

  /** Turns the value into a name. */
  def toName(a: A): String

  /** Looks up the value by name. */
  def fromName(name: String): Option[A] =
    byName.get(name)

  /** Lookup by name. */
  val byName: Map[String, A] =
    values.iterator.map(v => toName(v) -> v).toMap

  def circeKeyCodec: CirceKeyCodec[A] = CirceKeyCodec.instance(fromName)(toName)

  def schema: Schema[A] = Schema
    .string[A]
    .encodedExample(show""""${toName(values.head)}"""")
    .format(show"enum (${values.iterator.map(toName).mkString(" | ")})")
}
