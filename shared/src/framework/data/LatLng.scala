package framework.data

import framework.utils.NewtypeDouble

object Latitude extends NewtypeDouble
type Latitude = Latitude.Type

object Longitude extends NewtypeDouble
type Longitude = Longitude.Type

/** Latitude and longitude. */
case class LatLng(latitude: Latitude, longitude: Longitude)
