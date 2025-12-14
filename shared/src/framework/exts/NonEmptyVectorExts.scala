package framework.exts

import cats.data.NonEmptyVector
import alleycats.Empty
import cats.data.NonEmptySet
import scala.collection.immutable.SortedSet

extension [A](self: NonEmptyVector[A]) {

  /** As [[cats.data.NonEmptyVector.updatedUnsafe]] but takes in an update function.
    */
  def updatedWithUnsafe(idx: Int, update: A => A): NonEmptyVector[A] = {
    val currentValue = self.getUnsafe(idx)
    val newValue = update(currentValue)
    NonEmptyVector.fromVectorUnsafe(self.toVector.updated(idx, newValue))
  }

  /** Removes the element at the given index.
    *
    * @return
    *   [[None]] if last element is removed, [[Some]] if not.
    */
  def removeAt(idx: Int): Option[NonEmptyVector[A]] = {
    NonEmptyVector.fromVector(self.toVector.removeAt(idx))
  }

  /** Removes the element at the given index.
    *
    * @return
    *   if the last element was removed, returns a [[NonEmptyVector]] with a single element provided by [[Empty.empty]].
    */
  def removeAtOrDefault(idx: Int)(implicit
    empty: Empty[A]
  ): NonEmptyVector[A] =
    removeAt(idx).getOrElse(NonEmptyVector.one(empty.empty))

  def toNonEmptySet(using Ordering[A]): NonEmptySet[A] = NonEmptySet.fromSetUnsafe(SortedSet.from(self.iterator))
}
