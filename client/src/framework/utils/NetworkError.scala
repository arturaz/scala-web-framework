package framework.utils

import sttp.model.StatusCode
import sttp.tapir.DecodeResult

import scala.scalajs.js.JavaScriptException

/** Errors that can occur while sending a request. */
enum NetworkError derives CanEqual {

  /** Something went wrong while sending the request. */
  case JsError(jsError: JavaScriptException) extends NetworkError

  /** The response could not be parsed. */
  case DecodeError(failure: DecodeResult.Failure) extends NetworkError

  /** The server (or a proxy) rate-limited us (HTTP 429). Transient — the user should retry shortly. */
  case RateLimited(error: DecodeResult.Failure) extends NetworkError

  /** A non-success HTTP status (e.g. 502/503 from a proxy) whose body could not be decoded as the expected type.
    * Typically a proxy error page rather than an app-level error.
    */
  case UnexpectedResponse(statusCode: StatusCode, error: DecodeResult.Failure) extends NetworkError

  def asNetworkOrEndpointError: NetworkOrEndpointError[Nothing] = NetworkOrEndpointError.NetworkError(this)

  override def toString(): String = this match {
    // Make sure we show the chain of all causes of the error
    case DecodeError(DecodeResult.Error(original, error)) =>
      show"""|NetworkError.DecodeError(DecodeResult.Error(
             |  original: '$original'
             |  error(s):
             |${error.errorWithCausesAndStacktracesString.indent(4)}
             |))""".stripMargin

    case DecodeError(other) => s"NetworkError.DecodeError($other)"
    case JsError(err)       => s"NetworkError.JsError($err)"
    case RateLimited(error) =>
      show"NetworkError.RateLimited(${error.toString})"
    case UnexpectedResponse(statusCode, error) =>
      show"NetworkError.UnexpectedResponse(statusCode: $statusCode, error: ${error.toString})"
  }
}

enum NetworkRequestFailure derives CanEqual {
  case NetworkError(err: framework.utils.NetworkError)
  case Aborted

  def asNetworkOrEndpointError: AuthenticatedNetworkRequestFailure[Nothing] = this match {
    case NetworkRequestFailure.NetworkError(err) =>
      AuthenticatedNetworkRequestFailure.NetworkOrEndpointError(NetworkOrEndpointError.NetworkError(err))
    case Aborted => AuthenticatedNetworkRequestFailure.Aborted
  }
}
