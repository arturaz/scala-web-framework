package framework.prelude

import cats.kernel.{Order, Semigroup}
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.partial.Result.Errors

given identityTransformer[A]: Transformer[A, A] = a => a
// If this one is a `given` it somehow becomes ambiguous with `identityTransformer`, even though the types are
// different :/
def identityPartialTransformer[A]: PartialTransformer[A, A] = PartialTransformer(Result.fromValue)

given intToLongTransformer: Transformer[Int, Long] = _.toLong

// Instances that Chimney does not provide by default: https://github.com/scalalandio/chimney/issues/569

given nonEmptyVectorTransformer[From, To](using
  t: Transformer[From, To]
): Transformer[NonEmptyVector[From], NonEmptyVector[To]] =
  _.map(t.transform)

given nonEmptyListTransformer[From, To](using
  t: Transformer[From, To]
): Transformer[NonEmptyList[From], NonEmptyList[To]] =
  _.map(t.transform)

given nonEmptySetTransformer[A, B: Order](using t: Transformer[A, B]): Transformer[NonEmptySet[A], NonEmptySet[B]] =
  _.map(t.transform)

given Semigroup[Errors] = (a, b) => Errors(a.errors ++ b.errors)
