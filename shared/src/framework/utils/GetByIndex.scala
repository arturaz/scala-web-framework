package framework.utils

import cats.data.{NonEmptyList, NonEmptyVector}

trait GetByIndex {
  type Self
  type Element

  extension (self: Self) {
    def getByIndex(index: Int): Element
  }
}
object GetByIndex {
  type Of[CollectionType[_], ElementType] = GetByIndex {
    type Self = CollectionType[ElementType]
    type Element = ElementType
  }

  given forNonEmptyVector[A]: GetByIndex.Of[NonEmptyVector, A] = new GetByIndex {
    type Self = NonEmptyVector[A]
    type Element = A

    extension (self: Self) {
      def getByIndex(index: Int): Element = self.getUnsafe(index)
    }
  }

  given forNonEmptyList[A]: GetByIndex.Of[NonEmptyList, A] = new GetByIndex {
    type Self = NonEmptyList[A]
    type Element = A

    extension (self: Self) {
      def getByIndex(index: Int): Element = self.toList(index)
    }
  }

  given forSeq[A, Collection[X] <: Seq[X]]: GetByIndex.Of[Collection, A] = new GetByIndex {
    type Self = Collection[A]
    type Element = A

    extension (self: Self) {
      def getByIndex(index: Int): Element = self(index)
    }
  }
}
