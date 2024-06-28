package framework.exts

import sourcecode.*

extension [T, V <: SourceValue[T]](c: SourceCompanion[T, V]) {

  /** Summons a value.
    *
    * Example: `s"Enclosing type: ${sourcecode.Enclosing.here}"`
    */
  def here(implicit v: V): T = v.value
}
