package framework.exts

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}

extension (arrayBuffer: ArrayBuffer) {
  def toByteArray: Array[Byte] = Int8Array(arrayBuffer).toArray

  def toByteIArray: IArray[Byte] = IArray.unsafeFromArray(arrayBuffer.toByteArray)
}
