package framework.utils

import yantl.Newtype
import sttp.model.Uri
import framework.prelude.*
import framework.exts.*
import io.scalaland.chimney.Transformer

trait NewtypeUri extends Newtype.Of[Uri] {
  given Conversion[Type, Uri] = unwrap
  given Show[Type] = unwrap(_).toString
}
object NewtypeUri {

  /** Makes a [[Transformer]] that assumes you can always make a newtype from an [[Uri]]. */
  def unvalidatedTransformer(newtype: NewtypeUri): Transformer[Uri, newtype.Type] =
    uri => newtype.make(uri).getOrThrow
}
