package framework.exts

import sttp.tapir.DecodeResult

extension (obj: DecodeResult.Error.type) {
  def apply(msg: String): DecodeResult.Error = DecodeResult.Error(msg, new RuntimeException(msg))
}
