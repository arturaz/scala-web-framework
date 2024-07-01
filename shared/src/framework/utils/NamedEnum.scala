package framework.utils

import cats.data.NonEmptyVector
import cats.syntax.show.*
import framework.exts.fromKeyCodecs
import framework.prelude.*
import sttp.tapir.Schema

import scala.collection.immutable.SortedSet

/** Indicates that the enum has conversions from/to [[String]]. Similar to `enumeratum` library, but less boilerplatey
  * and plays nice with Scala 3 `enum`s.
  */
trait NamedEnum[A] {

  /** All values of the enum. */
  lazy val values: NonEmptyVector[A]

  /** [[values]] but as a set. Ordered by name. */
  lazy val valuesSet: NonEmptySet[A] =
    NonEmptySet.fromSetUnsafe(SortedSet.from(values.iterator)(using Ordering.by(toName)))

  /** Turns the value into a name. */
  def toName(a: A): String

  /** Looks up the value by name. */
  def fromName(name: String): Option[A] =
    byName.get(name)

  /** Lookup by name. */
  val byName: Map[String, A] =
    values.iterator.map(v => toName(v) -> v).toMap

  def circeKeyCodec: CirceKeyCodec[A] = CirceKeyCodec.instance(fromName)(toName)

  def circeCodec: CirceCodec[A] = {
    val keyCodec = circeKeyCodec
    CirceCodec.fromKeyCodecs(using keyCodec, keyCodec)
  }

  def schema: Schema[A] = Schema
    .string[A]
    .encodedExample(show""""${toName(values.head)}"""")
    .format(show"enum (${values.iterator.map(toName).mkString(" | ")})")

  def tapirCodec: TapirCodec[String, A, TapirCodecFormat.TextPlain] =
    TapirCodec.string.mapEither(str => fromName(str).toRight(show"Unknown value: $str"))(toName)

  def show: Show[A] = toName(_)
}
object NamedEnum {

  /** A [[NamedEnum]] that additionally can be converted from / to [[Int]]. */
  trait WithIntRepresentation[A] { self: NamedEnum[A] =>
    def toInt(a: A): Int

    def fromInt(i: Int): Option[A] =
      byInt.get(i)

    def fromIntE(i: Int): Either[String, A] =
      fromInt(i).toRight(show"Unknown value: $i")

    lazy val byInt: Map[Int, A] =
      values.iterator.map(v => toInt(v) -> v).toMap
  }
}
