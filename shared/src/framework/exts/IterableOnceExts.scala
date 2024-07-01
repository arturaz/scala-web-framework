package framework.exts

import scala.collection.immutable.SortedSet

extension [A](iter: IterableOnce[A]) {
  def toSortedSet(using Ordering[A]): SortedSet[A] = SortedSet.from(iter)
}
