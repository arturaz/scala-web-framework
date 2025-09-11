package framework.data

import cats.data.{Validated, ValidatedNel}
import cats.syntax.validated.*
import framework.exts.*
import framework.prelude.{*, given}
import framework.utils.{NewtypeDouble, NewtypeString}
import io.scalaland.chimney.{PartialTransformer, Transformer}
import sttp.model.Uri
import sttp.tapir.{Schema, SchemaType}
import yantl.*

object Latitude
    extends Newtype.ValidatedOf(
      Validator.of(
        ValidatorRule.between[Double](Latitude.MinValue, Latitude.MaxValue)
      )
    ),
      NewtypeDouble {
  final val MinValue: Double = -90
  final val MaxValue: Double = 90

  val zero = make.orThrow(0)
  given Schema[Type] =
    Schema(SchemaType.SNumber()).description(show"Latitude in degrees, between $MinValue and $MaxValue inclusive.")
}
type Latitude = Latitude.Type

object Longitude
    extends Newtype.ValidatedOf(
      Validator.of(
        ValidatorRule.between[Double](Longitude.MinValue, Longitude.MaxValue)
      )
    ),
      NewtypeDouble {
  final val MinValue: Double = -180
  final val MaxValue: Double = 180

  val zero = make.orThrow(0)
  given Schema[Type] =
    Schema(SchemaType.SNumber()).description(show"Longitude in degrees, between $MinValue and $MaxValue inclusive.")
}
type Longitude = Longitude.Type

/** Latitude and longitude. */
case class LatLng(latitude: Latitude, longitude: Longitude) derives CanEqual, Schema, CirceCodec {

  /** Formats as 54.992078, 23.687857. */
  def asString: String = s"${latitude.unwrap}, ${longitude.unwrap}"

  /** Returns an Uri for Google Maps for this location. */
  def googleMapsUri: Uri =
    Uri
      .parse("https://maps.google.com")
      .getOrThrow
      .addQuerySegmentKV("q", show"${latitude.unwrap},${longitude.unwrap}")
}
object LatLng {
  def zero: LatLng = apply(Latitude.zero, Longitude.zero)

  /** Regex for parsing a string in the form "54.992078, 23.687857". */
  val ParseRegex = """^(-?\d+\.\d+),\s*(-?\d+\.\d+)$""".r

  enum ParseError derives CanEqual {
    case LatitudeParseError(error: Latitude.TError)
    case LongitudeParseError(error: Longitude.TError)
    case InvalidFormatParseError
  }

  /** Parses a string in the form "54.992078, 23.687857". */
  def parse(s: String): ValidatedNel[ParseError, LatLng] = {
    s match {
      case ParseRegex(latStr, lngStr) =>
        val lat = latStr.toDouble
        val lng = lngStr.toDouble

        (
          Validated.fromEither(
            Latitude
              .make(lat)
              .left
              .map(vec => NonEmptyList.fromFoldable(vec.map(ParseError.LatitudeParseError(_))).getOrThrow("impossible"))
          ),
          Validated.fromEither(
            Longitude
              .make(lng)
              .left
              .map(vec =>
                NonEmptyList.fromFoldable(vec.map(ParseError.LongitudeParseError(_))).getOrThrow("impossible")
              )
          ),
        ).mapN(LatLng.apply)

      case _ => Validated.invalidNel(ParseError.InvalidFormatParseError)
    }
  }

  given makeFromString: Make[String, ParseError, LatLng] with {

    override def apply(input: String): Either[Vector[ParseError], LatLng] =
      parse(input).toEither.left.map(_.iterator.toVector)

    override def unsafe(input: String): LatLng = orThrow(input)
  }

  given asString: Transformer[LatLng, String] = _.asString

  given fromString: PartialTransformer[String, LatLng] =
    PartialTransformer.fromEitherStrings(str => LatLng.parse(str).toEither.left.map(_.toList.map(_.toString)))

  /** Newtype helper for data types that wrap [[LatLng]]. */
  trait Newtype extends Newtype.Of[LatLng] { self =>
    given schema: Schema[Type] = summon[Schema[LatLng]].map(v => self.make(v).toOption)(unwrap)
    given circeCodec: CirceCodec[Type] = summon[CirceCodec[LatLng]].iemap(self.make.asString)(unwrap)

    given transformerForUnvalidated(using
      newType: Newtype.WithUnvalidatedType[LatLng, Type]
    ): Transformer[LatLng, Type] = latLng => newType(latLng)
  }

  extension (obj: Newtype.WithUnderlying[LatLng] & Newtype.WithoutValidation) {
    def zero: obj.Type = obj.apply(LatLng.zero)
  }
}
