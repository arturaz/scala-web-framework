package framework.prelude

import framework.prelude.*
import framework.exts.*

/** Provides a [[Schema]] for newtypes if we have a [[Schema]] for their underlying type. */
given newTypeSchema[TUnderlying, TWrapper](using
  newType: yantl.Newtype.WithType[TUnderlying, TWrapper],
  schema: Schema[TUnderlying],
): Schema[TWrapper] =
  schema.map(newType.make(_).toOption)(newType.unwrap)

/** Provides a [[Schema]] for [[Map]]s where the key is a newtype that wraps [[String]]. */
given stringNewTypeMapSchema[TKey, TValue](using
  newType: yantl.Newtype.WithType[String, TKey],
  valueSchema: Schema[TValue],
): Schema[Map[TKey, TValue]] =
  Schema.schemaForMap[TKey, TValue](newType.unwrap)
