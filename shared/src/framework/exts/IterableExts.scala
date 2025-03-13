package framework.exts

import cats.kernel.Semigroup
import cats.syntax.semigroup.*
import scala.collection.Factory

extension [K, V](iterable: Iterable[(K, V)]) {

  /** Turns the [[Iterable]] to a [[Map]], merging the values using the [[Semigroup]] if same key is found. */
  def mergeToMap(using Semigroup[V]): Map[K, V] = {
    iterable.groupMapReduce(_._1)(_._2)(_ |+| _)
  }
}

extension [C[X] <: Iterable[X], A](iterable: C[A]) {

  /** Modifies the elements of the collection that match the `predicate` with the `modifier` function. */
  def modifyWith(predicate: A => Boolean)(modifier: A => A)(using factory: Factory[A, C[A]]): C[A] = {
    iterable.iterator
      .map(a => if (predicate(a)) modifier(a) else a)
      .to(factory)
  }
}
