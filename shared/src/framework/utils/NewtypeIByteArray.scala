package framework.utils

import framework.exts.*
import io.scalaland.chimney.Transformer
import yantl.Newtype

/** A newtype wrapping an [[IArray]] of [[Byte]]s. */
trait NewtypeIByteArray extends Newtype.WithoutValidationOf[IArray[Byte]] {
  given Transformer[Type, IArray[Byte]] = unwrap
  given Transformer[IArray[Byte], Type] = apply
}
