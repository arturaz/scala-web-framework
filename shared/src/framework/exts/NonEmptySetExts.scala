package framework.exts

import cats.data.NonEmptySet
import scala.collection.immutable.SortedSet

extension [A](set: NonEmptySet[A]) {

  /** Concatenates this [[NonEmptySet]] with the given [[IterableOnce]]. */
  def +++(other: IterableOnce[A]): NonEmptySet[A] = {
    NonEmptySet.fromSet(SortedSet.from(other)(using set.toSortedSet.ordering)) match {
      case Some(otherSet) => set ++ otherSet
      case None           => set
    }
  }
}
