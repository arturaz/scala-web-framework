package framework.prelude

import cats.kernel.{Order, Semigroup}
import io.scalaland.chimney.Transformer
import io.scalaland.chimney.partial.Result.Errors

/** An identity transformer. */
given identityTransformer[A]: Transformer[A, A] = a => a

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
