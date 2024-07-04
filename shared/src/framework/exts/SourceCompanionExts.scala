package framework.exts

import sourcecode.*

extension [T, V <: SourceValue[T]](c: SourceCompanion[T, V]) {

  /** Summons a value.
    *
    * Example: `s"Enclosing type: ${sourcecode.Enclosing.here}"`
    */
  def here(using v: V): T = v.value

  /** Summons a value and turns it into a string. */
  def hereShow(using v: V): String = v.value.toString()
}
