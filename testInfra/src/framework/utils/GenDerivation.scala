package framework.utils

import magnolia1.*
import hedgehog.Gen
import hedgehog.core.GenT

trait GenDerivation extends AutoDerivation[Gen] {
  given Monadic[Gen] with {
    override def point[A](value: A): GenT[A] = Gen.constant(value)
    override def map[A, B](from: GenT[A])(fn: A => B): GenT[B] = from.map(fn)
    override def flatMap[A, B](from: GenT[A])(fn: A => GenT[B]): GenT[B] = from.flatMap(fn)
  }

  override def join[T](caseClass: CaseClass[GenT, T]): GenT[T] = {
    caseClass.parameters.iterator
      .foldLeft(Gen.constant(Array.empty[Any]))((current, param) =>
        for {
          arr <- current
          value <- param.typeclass
        } yield arr :+ value
      )
      .map(arr => caseClass.rawConstruct(Seq.from(arr)))
  }

  override def split[T](sealedTrait: SealedTrait[GenT, T]): GenT[T] = {
    val gens = sealedTrait.subtypes.map(subtype => subtype.typeclass.map(value => value: T))
    Gen.choice1(gens.head, gens.tail*)
  }
}
object GenDerivation extends GenDerivation
