package framework.utils

import cats.data.{Chain, NonEmptyList, NonEmptyVector}

/** Typeclass for getting the first element in a collection. */
trait MaybeGetHead[-Collection, +Element] {
  def headOption(collection: Collection): Option[Element]
}
object MaybeGetHead {
  given [A, C[X] <: IndexedSeq[X]]: MaybeGetHead[C[A], A] = _.headOption
  given [A]: MaybeGetHead[List[A], A] = _.headOption
  given [A]: MaybeGetHead[NonEmptyVector[A], A] = c => Some(c.head)
  given [A]: MaybeGetHead[NonEmptyList[A], A] = c => Some(c.head)
  given [A]: MaybeGetHead[Chain[A], A] = _.headOption
}
