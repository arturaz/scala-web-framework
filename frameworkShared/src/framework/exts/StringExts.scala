package framework.exts

import java.nio.charset.StandardCharsets
import cats.data.NonEmptyVector

extension (s: String) {
  def toNonEmptyOption: Option[String] =
    if (s.nonEmpty) Some(s) else None

  /** Converts the string into a byte array encoded in UTF-8. */
  def utf8Bytes: IArray[Byte] =
    IArray.unsafeFromArray(s.getBytes(StandardCharsets.UTF_8))

  /** Returns the [[NonEmptyVector]] of [[String]] lines if the string is not empty. */
  def linesNonEmptyVector: Option[NonEmptyVector[String]] =
    NonEmptyVector.fromVector(s.linesIterator.toVector)

  /** Returns the string with the first character lowercased. */
  def firstCharToLower: String =
    s"${s.head.toLower}${s.tail}"

  /** Returns the string with the first character uppercased. */
  def firstCharToUpper: String =
    s"${s.head.toUpper}${s.tail}"
}
