package framework.api

import framework.utils.NewtypeBoolean
import framework.prelude.CanEqual1

/** Whether the request was successful or not. */
object RequestSuccessful extends NewtypeBoolean {
  def fromExpected[A: CanEqual1](expected: A)(actual: A): RequestSuccessful =
    if (expected == actual) apply(true) else apply(false)
}
type RequestSuccessful = RequestSuccessful.Type
