package framework.utils.postgis

import doobie.util.meta.Meta
import framework.prelude.given
import framework.data.{LatLng, Latitude, Longitude}
import net.postgis.jdbc.geometry.Point

/** Export from this to get [[Get]] and [[Put]] instances for [[LatLng]] which uses PostGIS [[Point]] internally. */
object WithLatLngBackedByPostGIS {
  given latLngMeta: Meta[LatLng] =
    doobie.postgres.pgisgeographyimplicits.PointType.imap(p =>
      LatLng(Latitude.make.orThrow(p.y), Longitude.make.orThrow(p.x))
    )(p => new Point(p.longitude.unwrap, p.latitude.unwrap))
}
