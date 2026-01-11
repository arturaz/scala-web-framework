package framework.prelude

import framework.exts.*
import scodec.{Attempt, Err}

given newTypeSCodecCodec[TUnderlying, TWrapper](using
  newType: yantl.Newtype.WithType[TUnderlying, TWrapper],
  codec: SCodecCodec[TUnderlying],
): scodec.Codec[TWrapper] =
  codec.exmap(
    underlying => Attempt.fromEither(newType.make.asString(underlying).left.map(Err.apply)),
    underlying => Attempt.successful(newType.unwrap(underlying)),
  )
