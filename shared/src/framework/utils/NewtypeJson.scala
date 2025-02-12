package framework.utils

import cats.Show
import framework.exts.*
import framework.prelude.*
import io.circe.Json
import io.scalaland.chimney.Transformer
import yantl.Newtype

/** A newtype wrapping a [[Json]]. */
trait NewtypeJson extends Newtype.Of[Json] {
  given Show[Type] = unwrap(_).noSpacesSortKeys
  given CanEqual[Type, Type] = CanEqual.derived
  given Transformer[Type, Json] = unwrap
}
