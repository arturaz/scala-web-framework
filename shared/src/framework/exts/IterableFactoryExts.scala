package framework.exts

import scala.collection.IterableFactory

extension [CC[_]](factory: IterableFactory[CC]) {

  /** Builds a collection if the `condition` is true.
    *
    * Example:
    * {{{
    * Seq.when(items.size > 1)("first", "second")
    * }}}
    */
  inline def when[A](inline condition: Boolean)(inline items: A*): CC[A] =
    if (condition) factory(items*) else factory.empty

  /** Builds a collection if the `condition` is false.
    *
    * Example:
    * {{{
    * Seq.unless(items.size > 1)("first", "second")
    * }}}
    */
  inline def unless[A](inline condition: Boolean)(inline items: A*): CC[A] =
    if (!condition) factory(items*) else factory.empty
}
