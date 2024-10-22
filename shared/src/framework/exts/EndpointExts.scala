package framework.exts

import sttp.tapir.Endpoint

extension [SecurityInput, Input, Output, AuthError, Requirements](
  e: Endpoint[SecurityInput, Input, AuthError, Output, Requirements]
) {
  def mapSecurityInConversion[Other](using
    inputToOther: Conversion[SecurityInput, Other],
    otherToInput: Conversion[Other, SecurityInput],
  ): Endpoint[Other, Input, AuthError, Output, Requirements] =
    e.mapSecurityIn(inputToOther.apply)(inputToOther.apply)
}
