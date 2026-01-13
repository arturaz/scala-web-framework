package framework.prelude

import framework.utils.UnvalidatedNewtypeOf
import yantl.Newtype
import scala.collection.immutable.SortedSet
import sttp.tapir.SchemaType
import sttp.tapir.Schema.SName
import sttp.tapir.DecodeResult

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

given [A: Schema]: Schema[NonEmptyList[A]] = summon[Schema[List[A]]].map(NonEmptyList.fromList)(_.toList)
given [A: Schema]: Schema[NonEmptyVector[A]] = summon[Schema[Vector[A]]].map(NonEmptyVector.fromVector)(_.toVector)
given [A: Schema]: Schema[NonEmptySet[A]] = summon[Schema[SortedSet[A]]].map(NonEmptySet.fromSet)(_.toSortedSet)

given schemaForNothing: Schema[Nothing] =
  Schema.any.name(SName("Nothing")).description("Nothing - no values will be accepted or produced.")

given tapirCodecForNothing[LowLevel, CodecFormat <: TapirCodecFormat]: TapirCodec[LowLevel, Nothing, CodecFormat] with {
  def encode(h: Nothing): LowLevel = ???
  def format: CodecFormat = ???
  def rawDecode(l: LowLevel): sttp.tapir.DecodeResult[Nothing] = ???
  def schema: Schema[Nothing] = schemaForNothing
}

given tapirCodecForUnit: TapirCodec[String, Unit, TapirCodecFormat.TextPlain] with {
  def encode(h: Unit): String = ""
  val format: TapirCodecFormat.TextPlain = TapirCodecFormat.TextPlain()
  def rawDecode(l: String): DecodeResult[Unit] = DecodeResult.Value(())
  def schema: Schema[Unit] = summon
}
