package framework.prelude

import io.scalaland.chimney.Transformer

/** An identity transformer. */
given identityTransformer[A]: Transformer[A, A] = a => a

given intToLongTransformer: Transformer[Int, Long] = _.toLong
