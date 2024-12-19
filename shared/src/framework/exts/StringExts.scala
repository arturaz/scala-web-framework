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

  /** Indents all the lines in the string. */
  def indentLines(count: Int, indentString: String = " ", indentFirstLine: Boolean = true): String = {
    val str = indentString * count
    s.linesIterator.zipWithIndex
      .map { case (line, idx) =>
        if (idx == 0 && !indentFirstLine) line
        else s"$str$line"
      }
      .mkString("\n")
  }

  /** As [[indentLines]] but doesn't indent first line. (NFL = No First Line) */
  def indentLinesNFL(count: Int, indentString: String = " "): String =
    indentLines(count, indentString, indentFirstLine = false)

  /** Parses and decodes a JSON string. */
  def parseAndDecodeAsJson[A](using decoder: io.circe.Decoder[A]): Either[io.circe.Error, A] =
    decoder.parseAndDecode(s)
}
