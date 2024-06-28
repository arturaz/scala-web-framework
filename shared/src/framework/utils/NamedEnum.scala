package framework.utils

import cats.data.NonEmptyVector

/** Indicates that the enum has conversions from/to [[String]]. Similar to `enumeratum` library, but less boilerplatey
  * and plays nice with Scala 3 `enum`s.
  */
trait NamedEnum[A] {
  def toName(a: A): String
  def fromName(name: String): Option[A]

  /** All values of the enum. */
  def values: NonEmptyVector[A]
}
