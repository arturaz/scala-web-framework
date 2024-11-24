package framework.sourcecode

import cats.syntax.show.*
import sourcecode.{File, FullName, Line}
import cats.Show

/** Full definition of a source code position. */
case class DefinedAt(file: File, line: Line, name: FullName) {
  override def toString = show"[${name.value} @ ${file.value}:${line.value}]"
}
object DefinedAt {
  given generate(using file: File, line: Line, name: FullName): DefinedAt =
    apply(file, line, name)

  given show: Show[DefinedAt] = _.toString
}
