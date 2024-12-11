package framework.prelude

import framework.utils.UnvalidatedNewtypeOf
import yantl.Newtype

export sttp.tapir.{Codec as TapirCodec, CodecFormat as TapirCodecFormat, Schema as TapirSchema}

/** Provides a [[Schema]] for newtypes if we have a [[Schema]] for their underlying type. */
given unvalidatedNewTypeSchema[TUnvalidatedWrapper, TUnderlying](using
  newType: UnvalidatedNewtypeOf.WithType[TUnvalidatedWrapper, ?, ?, TUnderlying],
  schema: Schema[TUnderlying],
): Schema[TUnvalidatedWrapper] =
  schema.map(underlying => Some(newType(underlying)))(newType.unwrap)

/** Provides a [[Schema]] for [[Map]]s where the key is a newtype that wraps [[String]]. */
given stringUnvalidatedNewTypeMapSchema[TKey, TValue](using
  newType: UnvalidatedNewtypeOf.WithType[TKey, ?, ?, String],
  valueSchema: Schema[TValue],
): Schema[Map[TKey, TValue]] =
  Schema.schemaForMap[TKey, TValue](newType.unwrap)
