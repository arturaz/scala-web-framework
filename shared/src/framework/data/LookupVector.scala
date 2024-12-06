package framework.data

import framework.prelude.*

/** Like [[SortedMap]] but the order is arbitrary. */
case class LookupVector[K, V](vector: Vector[V], toKey: V => K) {
  type Index = Int

  // IMPROVE: this can be optimized by not recomputing the map from scratch on modifications
  lazy val lookup: Map[K, (V, Index)] =
    vector.iterator.zipWithIndex.map { case tpl @ (v, idx) => toKey(v) -> tpl }.toMap

  def apply(index: Index): Option[V] = vector.lift(index)
  def get(key: K): Option[(V, Index)] = lookup.get(key)
  def getValue(key: K): Option[V] = lookup.get(key).map(_._1)
  def getIndex(key: K): Option[Index] = lookup.get(key).map(_._2)

  def +:(v: V): LookupVector[K, V] =
    LookupVector(v +: vector, toKey)

  def :+(v: V): LookupVector[K, V] =
    LookupVector(vector :+ v, toKey)

  def replaceAt(index: Index, v: V): LookupVector[K, V] =
    LookupVector(vector.updated(index, v), toKey)

  def isEmpty: Boolean = vector.isEmpty
  def nonEmpty: Boolean = vector.nonEmpty
}
object LookupVector {
  def schema[K, V: Schema](toKey: V => K): Schema[LookupVector[K, V]] =
    summon[Schema[Vector[V]]].map(vec => Some(apply(vec, toKey)))(_.vector)

  def circeCodec[K, V: CirceDecoder: CirceEncoder](toKey: V => K): CirceCodec[LookupVector[K, V]] =
    CirceCodec.from(
      summon[CirceDecoder[Vector[V]]].map(apply(_, toKey)),
      summon[CirceEncoder[Vector[V]]].contramap(_.vector),
    )
}
