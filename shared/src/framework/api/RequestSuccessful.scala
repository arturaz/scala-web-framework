package framework.api

import framework.utils.NewtypeBoolean
import framework.prelude.CanEqual1

/** Whether the request was successful or not. */
object RequestSuccessful extends NewtypeBoolean {
  def fromExpected[A: CanEqual1](expected: A)(actual: A): RequestSuccessful =
    if (expected == actual) apply(true) else apply(false)

  /** If the `Option` is `Some` then the request was successful, if the `Option` is `None` then the request was not
    * successful.
    */
  def fromOption(option: Option[Unit]): RequestSuccessful = option match {
    case Some(()) => apply(true)
    case None     => apply(false)
  }
}
type RequestSuccessful = RequestSuccessful.Type
