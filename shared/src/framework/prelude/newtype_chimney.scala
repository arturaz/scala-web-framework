package framework.prelude

import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.{PartialTransformer, Transformer}

/** Allows converting newtype values from their underlying type to a newtype using chimney. */
given newTypePartialTransformer[TUnderlying, TWrapped](using
  newType: yantl.Newtype.WithType[TUnderlying, TWrapped]
): PartialTransformer[TUnderlying, TWrapped] =
  PartialTransformer { from =>
    Result.fromEither(newType.makeAsStrings(from).left.map(errs => Result.Errors.fromStrings(errs.head, errs.tail*)))
  }

/** Allows converting newtype values from their wrapped type to the underlying type using chimney. */
given newTypeTotalTransformer[TWrapped, TUnderlying](using
  newType: yantl.Newtype.WithType[TUnderlying, TWrapped]
): Transformer[TWrapped, TUnderlying] =
  from => newType.unwrap(from)
