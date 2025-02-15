package framework.exts

import framework.utils.{MaybeGetHead, MaybeGetLast}
import scala.collection.Factory
import scala.reflect.ClassTag
import alleycats.Empty

extension [A](a: A) {
  def pprint: String = _root_.pprint.apply(a).render

  def pprintWithoutColors: String = _root_.pprint.apply(a).plainText

  /** Gets the first element in the collection. */
  def headOption[Element](using typeclass: MaybeGetHead[A, Element]): Option[Element] =
    typeclass.headOption(a)

  /** Gets the first element in the collection or returns an empty value. */
  def headOrEmpty[Element](using MaybeGetHead[A, Element], Empty[Element]): Element =
    a.headOption.getOrEmpty

  /** Gets the last element in the collection. */
  def lastOption[Element](using typeclass: MaybeGetLast[A, Element]): Option[Element] =
    typeclass.lastOption(a)

  /** Gets the last element in the collection or returns an empty value. */
  def lastOrEmpty[Element](using MaybeGetLast[A, Element], Empty[Element]): Element =
    a.lastOption.getOrEmpty
}

extension [A, C[X]](value: A | C[A]) {

  /** Deunionsizes the value into a collection if it is not already one.
    *
    * Quite useful in Laminar development.
    */
  def deunionizeSeq(using factory: Factory[A, C[A]], ct: ClassTag[A]): C[A] =
    value match {
      case a: A  => factory.newBuilder.addOne(a).result()
      case other => other.asInstanceOf[C[A]]
    }
}
