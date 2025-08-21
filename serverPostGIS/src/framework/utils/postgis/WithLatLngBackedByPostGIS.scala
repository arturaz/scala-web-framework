package framework.utils.postgis

import doobie.postgres.Text
import doobie.util.meta.Meta
import framework.data.{LatLng, Latitude, Longitude}
import framework.prelude.given
import net.postgis.jdbc.geometry.Point

/** Export from this to get [[Get]] and [[Put]] instances for [[LatLng]] which uses PostGIS [[Point]] internally. */
object WithLatLngBackedByPostGIS {
  extension (latLng: LatLng) {

    /** Converts [[LatLng]] to PostGIS [[Point]]. */
    def asPostgisPoint: Point = Point(latLng.longitude.unwrap, latLng.latitude.unwrap)
  }

  given latLngMeta: Meta[LatLng] =
    doobie.postgres.pgisgeographyimplicits.PointType.imap(p =>
      LatLng(Latitude.make.orThrow(p.y), Longitude.make.orThrow(p.x))
    )(_.asPostgisPoint)

  given postgisPointText: Text[Point] = Text[String].contramap(_.toString())
  given latLngText: Text[LatLng] = Text[Point].contramap(_.asPostgisPoint)
}
