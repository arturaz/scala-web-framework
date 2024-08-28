package framework.exts

import cats.data.{NonEmptyMap, NonEmptyVector}

implicit class NonEmptyMapExts[K, V](private val map: NonEmptyMap[K, V]) extends AnyVal {
  def toNonEmptyVector: NonEmptyVector[(K, V)] = NonEmptyVector.fromVectorUnsafe(map.toSortedMap.toVector)
}
