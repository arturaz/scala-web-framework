package framework.exts

import sttp.tapir.Endpoint
import io.scalaland.chimney.Transformer

extension [SecurityInput, Input, Output, AuthError, Requirements](
  e: Endpoint[SecurityInput, Input, AuthError, Output, Requirements]
) {
  def mapSecurityInConversion[Other](using
    inputToOther: Transformer[SecurityInput, Other],
    otherToInput: Conversion[Other, SecurityInput],
  ): Endpoint[Other, Input, AuthError, Output, Requirements] =
    e.mapSecurityIn(inputToOther.transform)(otherToInput.apply)
}
