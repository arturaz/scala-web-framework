package framework.prelude

import io.scalaland.chimney.Transformer
import cats.kernel.Order

/** An identity transformer. */
given identityTransformer[A]: Transformer[A, A] = a => a

given intToLongTransformer: Transformer[Int, Long] = _.toLong

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
