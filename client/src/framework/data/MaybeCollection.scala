package framework.data

import scala.collection.Factory
import scala.annotation.targetName

enum MaybeCollection[A, C[_]] {
  case Value(value: A)
  case Values(values: C[A])

  /** Turns this into a collection. */
  def deunionize(using factory: Factory[A, C[A]]): C[A] = this match {
    case Values(values) => values
    case Value(value)   => factory.newBuilder.addOne(value).result()
  }
}
object MaybeCollection {
  def apply[A, C[_]](a: A): MaybeCollection[A, C] = Value(a)

  @targetName("applyCollection")
  def apply[A, C[_]](values: C[A]): MaybeCollection[A, C] = Values(values)

  given [A, C[_]]: Conversion[A, MaybeCollection[A, C]] = apply
  given [A, C[_]]: Conversion[C[A], MaybeCollection[A, C]] = apply
}

type MaybeSeq[A] = MaybeCollection[A, Seq]
object MaybeSeq {
  def apply[A](a: A): MaybeSeq[A] = MaybeCollection(a)

  @targetName("applyCollection")
  def apply[A](values: Seq[A]): MaybeSeq[A] = MaybeCollection(values)
}

type MaybeVector[A] = MaybeCollection[A, Vector]
object MaybeVector {
  def apply[A](a: A): MaybeVector[A] = MaybeCollection(a)

  @targetName("applyCollection")
  def apply[A](values: Vector[A]): MaybeVector[A] = MaybeCollection(values)
}

type MaybeList[A] = MaybeCollection[A, List]
object MaybeList {
  def apply[A](a: A): MaybeList[A] = MaybeCollection(a)

  @targetName("applyCollection")
  def apply[A](values: List[A]): MaybeList[A] = MaybeCollection(values)
}

type MaybeSet[A] = MaybeCollection[A, Set]
object MaybeSet {
  def apply[A](a: A): MaybeSet[A] = MaybeCollection(a)

  @targetName("applyCollection")
  def apply[A](values: Set[A]): MaybeSet[A] = MaybeCollection(values)
}
