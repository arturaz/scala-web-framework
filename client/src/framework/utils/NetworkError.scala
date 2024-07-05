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

  override def toString(): String = this match
    // Make sure we show the chain of all causes of the error../mill
    case DecodeError(DecodeResult.Error(original, error)) =>
      show"""|NetworkError.DecodeError(DecodeResult.Error(
             |  original: '$original'
             |  error(s):
             |${error.errorWithCausesAndStacktracesString.indent(4)}
             |))""".stripMargin

    case DecodeError(other) => s"NetworkError.DecodeError($other)"
    case JsError(err)       => s"NetworkError.JsError($err)"
}
