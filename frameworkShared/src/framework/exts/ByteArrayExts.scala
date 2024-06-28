package framework.exts

import java.nio.charset.StandardCharsets

extension (arr: Array[Byte]) {

  /** Converts the byte array into a string encoded in UTF-8. */
  def utf8String: String = new String(arr, StandardCharsets.UTF_8)
}
