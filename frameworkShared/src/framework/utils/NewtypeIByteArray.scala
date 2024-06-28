package framework.utils

import framework.exts.*
import io.scalaland.chimney.Transformer
import neotype.Newtype

/** A newtype wrapping an [[IArray]] of [[Byte]]s. */
trait NewtypeIByteArray extends Newtype[IArray[Byte]] {
  given Transformer[Type, IArray[Byte]] = unwrap
  given Transformer[IArray[Byte], Type] = make(_).getOrThrow
}
