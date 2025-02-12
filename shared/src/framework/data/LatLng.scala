package framework.data

import framework.utils.NewtypeDouble
import framework.prelude.{*, given}
import sttp.tapir.{Schema, SchemaType}
import yantl.*

object Latitude extends NewtypeDouble {
  type TError = ValidatorRule.SmallerThan[Double] | ValidatorRule.LargerThan[Double]

  override val validator = Validator.of(
    ValidatorRule.between[Double](-90, 90)
  )

  val zero = makeOrThrow(0)
  given Schema[Type] = Schema(SchemaType.SNumber()).description("Latitude in degrees, between -90 and 90 inclusive.")
}
type Latitude = Latitude.Type

object Longitude extends NewtypeDouble {
  type TError = ValidatorRule.SmallerThan[Double] | ValidatorRule.LargerThan[Double]

  override val validator = Validator.of(
    ValidatorRule.between[Double](-180, 180)
  )

  val zero = makeOrThrow(0)
  given Schema[Type] = Schema(SchemaType.SNumber()).description("Longitude in degrees, between -180 and 180 inclusive.")
}
type Longitude = Longitude.Type

/** Latitude and longitude. */
case class LatLng(latitude: Latitude, longitude: Longitude) derives CanEqual, Schema, CirceCodec
object LatLng {
  def zero: LatLng = apply(Latitude.zero, Longitude.zero)

  trait Newtype extends Newtype.WithoutValidationOf[LatLng] { self =>
    val zero: Type = self.apply(LatLng.zero)

    given schema: Schema[Type] = summon[Schema[LatLng]].map(v => Some(self.apply(v)))(unwrap)
    given circeCodec: CirceCodec[Type] = summon[CirceCodec[LatLng]].imap(self.apply)(unwrap)
  }
}
