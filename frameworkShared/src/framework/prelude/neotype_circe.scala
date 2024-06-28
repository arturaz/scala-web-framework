package framework.prelude

import framework.prelude.*
import framework.exts.*

given newTypeCirceCodec[TUnderlying, TWrapper](using
  newType: neotype.Newtype.WithType[TUnderlying, TWrapper],
  encoder: CirceEncoder[TUnderlying],
  decoder: CirceDecoder[TUnderlying],
): CirceCodec[TWrapper] =
  CirceCodec.from(decoder, encoder).iemap(newType.make)(newType.unwrap)

given newTypeStringKeyDecoder[TWrapper](using
  newType: neotype.Newtype.WithType[String, TWrapper]
): CirceKeyDecoder[TWrapper] =
  CirceKeyDecoder.instance(newType.make(_).toOption)

given newTypeStringKeyEncoder[TWrapper](using
  newType: neotype.Newtype.WithType[String, TWrapper]
): CirceKeyEncoder[TWrapper] =
  CirceKeyEncoder.instance(newType.unwrap)
