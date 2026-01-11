package framework.prelude

import framework.exts.*

given newTypeCirceCodec[TUnderlying, TWrapper](using
  newType: yantl.Newtype.WithType[TUnderlying, TWrapper],
  encoder: CirceEncoder[TUnderlying],
  decoder: CirceDecoder[TUnderlying],
): CirceCodec[TWrapper] =
  CirceCodec.from(decoder, encoder).iemap(newType.make.asString)(newType.unwrap)

given newTypeStringKeyDecoder[TWrapper](using
  newType: yantl.Newtype.WithType[String, TWrapper]
): CirceKeyDecoder[TWrapper] =
  CirceKeyDecoder.instance(newType.make(_).toOption)

given newTypeStringKeyEncoder[TWrapper](using
  newType: yantl.Newtype.WithType[String, TWrapper]
): CirceKeyEncoder[TWrapper] =
  CirceKeyEncoder.instance(newType.unwrap)
