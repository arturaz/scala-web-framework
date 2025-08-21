package framework.exts

import framework.data.LatLng
import framework.db.{*, given}
import org.postgresql.geometric.PGpoint

extension (latLng: LatLng) {

  /** Returns a fragment that makes a PostGIS point from the latitude and longitude.
    *
    * @see
    *   https://postgis.net/docs/ST_MakePoint.html
    */
  def sqlStMakePoint: Fragment =
    Fragment.const0(show"ST_MakePoint(${latLng.longitude.toDouble}, ${latLng.latitude.toDouble})")

  /** As [[sqlStMakePoint]] but casts the result to `geography` type. */
  def sqlStMakePointGeography: Fragment = sql"$sqlStMakePoint::geography"

  def asPgPoint: PGpoint =
    PGpoint(latLng.longitude.unwrap, latLng.latitude.unwrap)
}
