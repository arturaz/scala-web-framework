package framework.utils

import cats.data.NonEmptyVector

/** Typeclass for getting the last element in a collection. */
trait MaybeGetLast[-Collection, +Element] {
  def lastOption(collection: Collection): Option[Element]
}
object MaybeGetLast {
  given [A, C[X] <: IndexedSeq[X]]: MaybeGetLast[C[A], A] = _.lastOption
  given [A]: MaybeGetLast[NonEmptyVector[A], A] = c => Some(c.last)
}
