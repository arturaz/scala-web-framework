package framework.prelude

import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.Transformer

/** Allows converting neotype values from their underlying type to a newtype using chimney. */
given newTypePartialTransformer[TUnderlying, TWrapped](using
  newType: neotype.Newtype.WithType[TUnderlying, TWrapped]
): PartialTransformer[TUnderlying, TWrapped] =
  PartialTransformer { from =>
    Result.fromEitherString(newType.make(from))
  }

/** Allows converting neotype values from their wrapped type to the underlying type using chimney. */
given newTypeTotalTransformer[TWrapped, TUnderlying](using
  newType: neotype.Newtype.WithType[TUnderlying, TWrapped]
): Transformer[TWrapped, TUnderlying] =
  from => newType.unwrap(from)
