package framework.exts

import com.raquo.airstream.custom.CustomSource
import framework.data.AppBaseUri
import framework.prelude.sttpClientInterpreter
import framework.utils.{NetworkError, NetworkOrAuthError}
import org.scalajs.dom.{EventSource, EventSourceInit, MessageEvent}
import sttp.client3.Request
import sttp.tapir.{DecodeResult, Endpoint, PublicEndpoint}

import scala.annotation.targetName
import scala.scalajs.js.JSON

extension [Input, Error, Output, Requirements](e: PublicEndpoint[Input, Error, Output, Requirements]) {
  def toReq(params: Input)(using baseUri: AppBaseUri): Request[Either[Error, Output], Requirements] = {
    sttpClientInterpreter
      .toRequestThrowDecodeFailures(e, Some(baseUri.uri))
      .apply(params)
  }
}

/** Public infallible endpoint extensions. */
extension [Input, Output, Requirements](e: PublicEndpoint[Input, Nothing, Output, Requirements]) {
  @targetName("toReqPublicInfallible")
  def toReq(params: Input)(using baseUri: AppBaseUri): Request[Either[NetworkError, Output], Requirements] = {
    sttpClientInterpreter
      .toRequest[Input, Nothing, Output, Requirements](e, Some(baseUri.uri))
      .apply(params)
      .mapResponse {
        case failure: DecodeResult.Failure => Left(NetworkError.DecodeError(failure): NetworkError)
        case DecodeResult.Value(either)    => either
      }
  }
}

extension [SecurityInput, Input, Output, AuthError, Requirements](
  e: Endpoint[SecurityInput, Input, AuthError, Output, Requirements]
) {
  def toReq(
    securityParams: SecurityInput,
    params: Input,
  )(using baseUri: AppBaseUri): Request[Either[NetworkOrAuthError[AuthError], Output], Requirements] = {
    sttpClientInterpreter
      .toSecureRequest(e, Some(baseUri.uri))
      .apply(securityParams)
      .apply(params)
      .mapResponse {
        case failure: DecodeResult.Failure   => Left(NetworkOrAuthError.NetworkError(NetworkError.DecodeError(failure)))
        case DecodeResult.Value(Left(error)) => Left(NetworkOrAuthError.AuthError(error))
        case DecodeResult.Value(Right(output)) => Right(output)
      }
  }

  /** Turns the endpoint into a server-sent event stream without decoding.
    *
    * @param withCredentials
    *   A boolean value, defaulting to false, indicating if CORS should be set to include credentials.
    */
  def toSSEStreamRaw(
    securityParams: SecurityInput,
    params: Input,
    withCredentials: js.UndefOr[Boolean] = js.undefined,
  )(using baseUri: AppBaseUri): EventStream[MessageEvent] = {
    val uri = e.toReq(securityParams, params).uri
    val options = js.Dynamic.literal(withCredentials = withCredentials).asInstanceOf[EventSourceInit]
    EventStream.fromDomEventSource(new EventSource(uri.toString, options))
  }

  /** Turns the endpoint into a server-sent event stream.
    *
    * Decoding failures will throw an exception.
    *
    * @param withCredentials
    *   A boolean value, defaulting to false, indicating if CORS should be set to include credentials.
    */
  def toSSEStream(
    securityParams: SecurityInput,
    params: Input,
    withCredentials: js.UndefOr[Boolean] = js.undefined,
  )(using
    baseUri: AppBaseUri,
    codec: TapirCodec[String, Output, TapirCodecFormat.TextPlain],
  ): EventStream[(MessageEvent, Output)] = {
    val evtStream = toSSEStreamRaw(securityParams, params, withCredentials)
    evtStream.map { evt =>
      val either = for {
        dataStr <- evt.data match {
          case str: String => Right(str)
          case other       => Left(s"Event data is not a string: $other")
        }
        output <- codec.decode(dataStr) match {
          case DecodeResult.Value(output)    => Right(output)
          case failure: DecodeResult.Failure => Left(s"Failed to decode event data '$dataStr': $failure")
        }
      } yield (evt, output)

      either match {
        case Left(err)    => throw new Exception(err)
        case Right(value) => value
      }
    }
  }
}
