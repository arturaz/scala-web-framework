package framework.data

import yantl.Newtype
import framework.utils.NewtypeDouble
import yantl.ValidatorRule

object Latitude extends NewtypeDouble {
  type TError = ValidatorRule.SmallerThan[Double] | ValidatorRule.LargerThan[Double]

  override val validatorRules = IArray(
    ValidatorRule.between[Double](-90, 90)
  )

  val zero = makeOrThrow(0)
}
type Latitude = Latitude.Type

object Longitude extends NewtypeDouble {
  type TError = ValidatorRule.SmallerThan[Double] | ValidatorRule.LargerThan[Double]

  override val validatorRules = IArray(
    ValidatorRule.between[Double](-180, 180)
  )

  val zero = makeOrThrow(0)
}
type Longitude = Longitude.Type

/** Latitude and longitude. */
case class LatLng(latitude: Latitude, longitude: Longitude)
object LatLng {
  def zero: LatLng = apply(Latitude.zero, Longitude.zero)
}
