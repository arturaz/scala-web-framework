package framework.utils

import cats.data.{NonEmptyList, NonEmptyVector}
import scala.collection.immutable.SeqOps

trait SetByIndex {
  type Self
  type Element

  extension (self: Self) {
    def setByIndex(index: Int, element: Element): Self
  }
}
object SetByIndex {
  type Of[CollectionType[_], ElementType] = SetByIndex {
    type Self = CollectionType[ElementType]
    type Element = ElementType
  }

  given forNonEmptyVector[A]: SetByIndex.Of[NonEmptyVector, A] = new SetByIndex {
    type Self = NonEmptyVector[A]
    type Element = A

    extension (self: Self) {
      def setByIndex(index: Int, element: Element): Self = self.updatedUnsafe(index, element)
    }
  }

  given forNonEmptyList[A]: SetByIndex.Of[NonEmptyList, A] = new SetByIndex {
    type Self = NonEmptyList[A]
    type Element = A

    extension (self: Self) {
      def setByIndex(index: Int, element: Element): Self =
        NonEmptyList.fromListUnsafe(self.toList.updated(index, element))
    }
  }

  given forSeq[A, Collection[X] <: SeqOps[X, Collection, Collection[X]]]: SetByIndex.Of[Collection, A] =
    new SetByIndex {
      type Self = Collection[A]
      type Element = A

      extension (self: Self) {
        def setByIndex(index: Int, element: Element): Self = self.updated(index, element)
      }
    }
}
