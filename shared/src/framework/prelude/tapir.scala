package framework.prelude

import framework.utils.UnvalidatedNewtypeOf
import neotype.Newtype

export sttp.tapir.{Codec as TapirCodec, CodecFormat as TapirCodecFormat, Schema as TapirSchema}

/** Provides a [[Schema]] for newtypes if we have a [[Schema]] for their underlying type. */
given unvalidatedNewTypeSchema[TUnvalidatedWrapper, TValidatedWrapperCompanion <: Newtype[TUnderlying], TUnderlying](
  using
  newType: UnvalidatedNewtypeOf.WithType[TUnvalidatedWrapper, TValidatedWrapperCompanion, TUnderlying],
  schema: Schema[TUnderlying],
): Schema[TUnvalidatedWrapper] =
  schema.map(underlying => Some(newType(underlying)))(newType.unwrap)

/** Provides a [[Schema]] for [[Map]]s where the key is a newtype that wraps [[String]]. */
given stringUnvalidatedNewTypeMapSchema[TKey, TValidatedWrapperCompanion <: Newtype[String], TValue](using
  newType: UnvalidatedNewtypeOf.WithType[TKey, TValidatedWrapperCompanion, String],
  valueSchema: Schema[TValue],
): Schema[Map[TKey, TValue]] =
  Schema.schemaForMap[TKey, TValue](newType.unwrap)
