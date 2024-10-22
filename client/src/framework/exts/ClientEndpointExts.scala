package framework.exts

import framework.data.AppBaseUri
import framework.prelude.sttpClientInterpreter
import framework.utils.{NetworkError, NetworkOrAuthError}
import sttp.client3.Request
import sttp.tapir.{DecodeResult, Endpoint, PublicEndpoint}

import scala.annotation.targetName

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
}
