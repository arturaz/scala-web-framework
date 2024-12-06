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
import framework.tapir.capabilities.ServerSentEvents
import sttp.model.Uri
import org.scalajs.dom.ErrorEvent
import framework.sourcecode.DefinedAt
import scala.concurrent.duration.*

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

trait OnSSEStreamError {

  /** Specifies the backoff function between retries. */
  def waitFor(index: Int): FiniteDuration

  def onError(uri: Uri, error: ErrorEvent, waitingFor: FiniteDuration)(using DefinedAt): Unit
}
object OnSSEStreamError {
  def apply(
    waitFor: Int => FiniteDuration,
    onError: (Uri, ErrorEvent, FiniteDuration) => DefinedAt ?=> Unit,
  )(using DefinedAt): OnSSEStreamError = {
    val _wait = waitFor
    val handler = onError

    new {
      override def waitFor(index: Int): FiniteDuration = _wait(index)

      override def onError(uri: Uri, error: ErrorEvent, waitingFor: FiniteDuration)(using definedAt: DefinedAt): Unit =
        handler(uri, error, waitingFor)(using definedAt)
    }
  }

  val default: OnSSEStreamError = apply(
    index => index.seconds,
    (uri, error, waitingFor) =>
      logError(
        s"Error in SSE stream (url=$uri), waiting for ${waitingFor.prettyFractional} before reconnecting: ",
        error,
      ),
  )
}

// TODO: make this actually fail to compile if the endpoint doesn't have ServerSentEvents capability
// https://softwaremill.community/t/introducing-serversentevents-capability-failing-to-achieve-type-safety/460
extension [SecurityInput, Input, Output, AuthError, Requirements <: ServerSentEvents](
  e: Endpoint[SecurityInput, Input, AuthError, Output, Requirements]
) {

  /** Turns the endpoint into a server-sent event stream without decoding.
    *
    * @param reconnectOnError
    *   If [[Some]] then when an error occurs the stream will be reconnected.
    * @param withCredentials
    *   A boolean value, defaulting to false, indicating if CORS should be set to include credentials.
    */
  def toSSEStreamRaw(
    securityParams: SecurityInput,
    params: Input,
    reconnectOnError: Option[OnSSEStreamError] = Some(OnSSEStreamError.default),
    withCredentials: js.UndefOr[Boolean] = js.undefined,
  )(using baseUri: AppBaseUri, definedAt: DefinedAt): EventStream[MessageEvent] = {
    val uri = e.toReq(securityParams, params).uri
    val options = js.Dynamic.literal(withCredentials = withCredentials).asInstanceOf[EventSourceInit]

    def create = new EventSource(uri.toString, options)

    reconnectOnError match {
      case None =>
        EventStream.fromDomEventSource(create)

      case Some(onError) =>
        def stream(index: Int): EventStream[MessageEvent] =
          EventStream.fromDomEventSourceEither(create).flatMapSwitch {
            case Right(value) => EventStream.fromValue(value)
            case Left(error) =>
              val wait = onError.waitFor(index)
              onError.onError(uri, error, wait)
              EventStream.delay(wait.toMillis.toInt).flatMapSwitch(_ => stream(index + 1))
          }

        stream(0)
    }
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
    reconnectOnError: Option[OnSSEStreamError] = Some(OnSSEStreamError.default),
    withCredentials: js.UndefOr[Boolean] = js.undefined,
  )(using
    baseUri: AppBaseUri,
    codec: TapirCodec[String, Output, ?],
    definedAt: DefinedAt,
  ): EventStream[(MessageEvent, Output)] = {
    val evtStream = toSSEStreamRaw(securityParams, params, reconnectOnError, withCredentials)
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
