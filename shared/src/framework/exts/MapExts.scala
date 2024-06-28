package framework.exts

import cats.data.NonEmptyVector

extension [K, V](map: Map[K, Option[V]]) {

  /** Asserts that all values in the map are [[Some]]. Returns [[Right]] in that case, otherwise returns [[Left]] with
    * all the missing keys turned into errors.
    */
  def assertAllAreSome[Err](keyToError: K => Err): Either[NonEmptyVector[Err], Map[K, V]] = {
    val (missingKeysIterable, resultsIterable) = map.partitionMap {
      case (k, None)    => Left(keyToError(k))
      case (k, Some(v)) => Right(k -> v)
    }

    NonEmptyVector.fromVector(missingKeysIterable.toVector).toLeft(resultsIterable.toMap)
  }
}

extension [K, V](map: Map[K, V]) {

  /** Returns [[None]] if [[Map]] has 0 entries, [[Some]] if it has 1 entry, throws an exception otherwise. */
  def zeroOrOneOrThrow: Option[V] = {
    if (map.isEmpty) None
    else if (map.sizeIs == 1) Some(map.head._2)
    else throw new IllegalStateException(s"Expected to have 0 or 1 entry, had ${map.size} entries.")
  }

  /** As [[Map.apply]] but throws an exception with a better error message. */
  def getUnsafe(key: K): V =
    map.get(key).getOrElse(throw new NoSuchElementException(s"Missing entry for key: $key"))
}