package framework.exts

import scala.collection.immutable.SortedSet

extension [A](iter: IterableOnce[A]) {
  def toSortedSet(using Ordering[A]): SortedSet[A] = SortedSet.from(iter)

  /** As [[IterableOnce.find]] but returns an [[Option]] of the value and its index. */
  def findWithIndex(f: A => Boolean): Option[(A, Int)] =
    iter.iterator.zipWithIndex.find { case (a, _) => f(a) }

  def findMap[B](f: A => Option[B]): Option[B] =
    iter.iterator.map(f).find(_.isDefined).flatten
}
