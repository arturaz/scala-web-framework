package framework.exts

import cats.kernel.Semigroup
import cats.syntax.semigroup.*

extension [K, V](iterable: Iterable[(K, V)]) {

  /** Turns the [[Iterable]] to a [[Map]], merging the values using the [[Semigroup]] if same key is found. */
  def mergeToMap(using Semigroup[V]): Map[K, V] = {
    iterable.groupMapReduce(_._1)(_._2)(_ |+| _)
  }
}
