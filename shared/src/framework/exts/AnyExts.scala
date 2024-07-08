package framework.exts

import framework.utils.{MaybeGetHead, MaybeGetLast}

extension [A](a: A) {
  def pprint: String = _root_.pprint.apply(a).render

  /** Gets the first element in the collection. */
  def headOption[Element](using typeclass: MaybeGetHead[A, Element]): Option[Element] =
    typeclass.headOption(a)

  /** Gets the last element in the collection. */
  def lastOption[Element](using typeclass: MaybeGetLast[A, Element]): Option[Element] =
    typeclass.lastOption(a)
}
