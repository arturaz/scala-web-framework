package framework.utils

import sttp.tapir.DecodeResult

import scala.scalajs.js.JavaScriptException

/** Errors that can occur while sending a request. */
enum NetworkError {

  /** Something went wrong while sending the request. */
  case JsError(jsError: JavaScriptException) extends NetworkError

  /** The response could not be parsed. */
  case DecodeError(failure: DecodeResult.Failure) extends NetworkError

  def asNetworkOrAuthError: NetworkOrAuthError[Nothing] = NetworkOrAuthError.NetworkError(this)
}
