package framework.sourcecode

import cats.syntax.show.*
import sourcecode.{File, FullName, Line}

/** Full definition of a source code position. */
case class DefinedAt(file: File, line: Line, name: FullName) {
  override def toString = s"[${name.value.show} @ ${file.value.show}:${line.value.show}]"
}
object DefinedAt {
  given generate(using file: File, line: Line, name: FullName): DefinedAt =
    apply(file, line, name)
}
