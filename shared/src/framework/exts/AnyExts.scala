package framework.exts

import framework.utils.{MaybeGetHead, MaybeGetLast}
import scala.collection.Factory
import scala.reflect.ClassTag
import alleycats.Empty

extension [A](a: A) {

  /** Returns a pretty-printed version of the value as [[_root_.fansi.Str]]. */
  inline def pprintFansi: _root_.fansi.Str = _root_.pprint.apply(a)

  /** Returns a pretty-printed version of the value as String with ANSI color codes. */
  inline def pprint: String = a.pprintFansi.render

  inline def pprintWithoutColors: String = a.pprintFansi.plainText

  /** Runs the function on the value and returns the original. */
  inline def tap[B](f: A => B): A = { f(a); a }

  /** Pipes the value into the function and returns the result. */
  inline def pipe[B](f: A => B): B = f(a)

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
