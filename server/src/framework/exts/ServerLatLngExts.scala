package framework.exts

import framework.data.LatLng
import framework.db.{*, given}

extension (latLng: LatLng) {
  def sqlStMakePoint = sql"ST_MakePoint(${latLng.longitude}, ${latLng.latitude})::geography"
}
