package framework.exts

import com.raquo.airstream.custom.CustomSource
import framework.api.FrameworkHeaders
import framework.data.{AppBaseUri, FrameworkDateTime}
import framework.prelude.sttpClientInterpreter
import framework.sourcecode.DefinedAt
import framework.tapir.capabilities.ServerSentEvents
import framework.utils.{NetworkError, NetworkOrAuthError, PrettyPrintDuration}
import org.scalajs.dom.{Event, EventSource, EventSourceInit, MessageEvent}
import sttp.client3.Request
import sttp.model.Uri
import sttp.tapir.{DecodeResult, Endpoint, PublicEndpoint}

import scala.annotation.targetName
import scala.concurrent.duration.*
import scala.scalajs.js.JSON

extension [Input, Error, Output, Requirements](e: PublicEndpoint[Input, Error, Output, Requirements]) {
  def toReq(params: Input, now: FrameworkDateTime)(using
    baseUri: AppBaseUri
  ): Request[Either[Error, Output], Requirements] = {
    sttpClientInterpreter
      .toRequestThrowDecodeFailures(e, Some(baseUri.uri))
      .apply(params)
      .withClientRequestTracing(now)
  }
}

/** Public infallible endpoint extensions. */
extension [Input, Output, Requirements](e: PublicEndpoint[Input, Nothing, Output, Requirements]) {
  @targetName("toReqPublicInfallible")
  def toReq(params: Input, now: FrameworkDateTime)(using
    baseUri: AppBaseUri
  ): Request[Either[NetworkError, Output], Requirements] = {
    sttpClientInterpreter
      .toRequest[Input, Nothing, Output, Requirements](e, Some(baseUri.uri))
      .apply(params)
      .withClientRequestTracing(now)
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
    now: FrameworkDateTime,
  )(using baseUri: AppBaseUri): Request[Either[NetworkOrAuthError[AuthError], Output], Requirements] = {
    sttpClientInterpreter
      .toSecureRequest(e, Some(baseUri.uri))
      .apply(securityParams)
      .apply(params)
      .withClientRequestTracing(now)
      .mapResponse {
        case failure: DecodeResult.Failure   => Left(NetworkOrAuthError.NetworkError(NetworkError.DecodeError(failure)))
        case DecodeResult.Value(Left(error)) => Left(NetworkOrAuthError.AuthError(error))
        case DecodeResult.Value(Right(output)) => Right(output)
      }
  }

  /** [[toReq]] with the current time. */
  def toReqNow(securityParams: SecurityInput, params: Input)(using
    AppBaseUri
  ): SyncIO[Request[Either[NetworkOrAuthError[AuthError], Output], Requirements]] =
    FrameworkDateTime.nowIO.map(e.toReq(securityParams, params, _))
}

trait OnSSEStreamError[SecurityInput, Input, -Message] { self =>

  /** When the stream receives a message, it is passed to this method.
    *
    * The rationale is that advanced stream handlers may want to do something with the message, for example a chat
    * client may only want to request unread messages from the server.
    */
  def onMessage(msg: Message): Unit

  /** Called when an error occurs.
    *
    * @param uri
    *   the [[Uri]] for which the failure occurred
    * @param connectionIndex
    *   the index of the connection that failed. Starts from 0.
    * @return
    *   the new [[SecurityInput]] and [[Input]] to use for the next request, and the time to wait before trying again.
    */
  def onError(
    uri: Uri,
    error: Event,
    connectionIndex: Int,
    securityInput: SecurityInput,
    input: Input,
  )(using
    DefinedAt
  ): (SecurityInput, Input, FiniteDuration)

  /** Allows you to transform the type of the message. */
  def withOnMessage[Message1](f: Message1 => Unit): OnSSEStreamError[SecurityInput, Input, Message1] = new {
    override def onMessage(msg: Message1): Unit = f(msg)

    override def onError(
      uri: Uri,
      error: Event,
      connectionIndex: Int,
      securityInput: SecurityInput,
      input: Input,
    )(using DefinedAt): (SecurityInput, Input, FiniteDuration) = self.onError(
      uri = uri,
      error = error,
      connectionIndex = connectionIndex,
      securityInput = securityInput,
      input = input,
    )
  }
}
object OnSSEStreamError {

  /** Just reconnects on error to the same endpoint. */
  def default[SecurityInput, Input, Message]: OnSSEStreamError[SecurityInput, Input, Message] = new {
    override def onMessage(msg: Message): Unit = {}

    override def onError(
      uri: Uri,
      error: Event,
      connectionIndex: Int,
      securityInput: SecurityInput,
      input: Input,
    )(using DefinedAt): (SecurityInput, Input, FiniteDuration) = defaultOnError(
      uri = uri,
      error = error,
      connectionIndex = connectionIndex,
      securityInput = securityInput,
      input = input,
    )
  }

  /** Stores the last message received and uses that to determine the next request when the error occurs. */
  def keepingLastMessage[SecurityInput, Input, Message, StoredContents](
    onMessage: (Option[StoredContents], Message) => Option[StoredContents],
    onError: (Uri, Event, Int, SecurityInput, Input, Option[StoredContents]) => DefinedAt ?=> (
      SecurityInput,
      Input,
      FiniteDuration,
    ),
    clearContentsOnError: Boolean = false,
  ): OnSSEStreamError[SecurityInput, Input, Message] = {
    val _onMessage = onMessage
    val _onError = onError

    new {
      private var _contents: Option[StoredContents] = None

      override def onMessage(msg: Message): Unit = {
        _contents = _onMessage(_contents, msg)
      }

      override def onError(
        uri: Uri,
        error: Event,
        connectionIndex: Int,
        securityInput: SecurityInput,
        input: Input,
      )(using DefinedAt): (SecurityInput, Input, FiniteDuration) = {
        val result = _onError(uri, error, connectionIndex, securityInput, input, _contents)
        if (clearContentsOnError) _contents = None
        result
      }
    }
  }

  def defaultWaitFor(index: Int): FiniteDuration =
    index.seconds

  def defaultOnError[SecurityInput, Input](
    uri: Uri,
    error: Event,
    connectionIndex: Int,
    securityInput: SecurityInput,
    input: Input,
  )(using definedAt: DefinedAt): (SecurityInput, Input, FiniteDuration) = {
    given PrettyPrintDuration.Strings = PrettyPrintDuration.Strings.EnglishShortNoSpaces
    val waitingFor = defaultWaitFor(connectionIndex)

    log.error(
      s"Error in SSE stream (url=$uri, index=$connectionIndex), waiting for ${waitingFor.prettyForDebug} " +
        s"before reconnecting.",
      s"[securityInput=$securityInput]",
      s"[input=$input]",
      "error=",
      error,
    )

    (securityInput, input, waitingFor)
  }
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
    now: () => FrameworkDateTime = FrameworkDateTime.now,
    reconnectOnError: Option[OnSSEStreamError[SecurityInput, Input, MessageEvent]] = Some(
      OnSSEStreamError.default[SecurityInput, Input, MessageEvent]
    ),
    withCredentials: Boolean = false,
  )(using baseUri: AppBaseUri, definedAt: DefinedAt): EventStream[MessageEvent] = {
    val options = js.Dynamic.literal(withCredentials = withCredentials).asInstanceOf[EventSourceInit]

    def create(uri: Uri) = {
      val uriStr = uri.toString
      log.debug(s"Creating SSE stream for $uri, options=", options)
      new EventSource(uriStr, options)
    }
    def createUri(securityParams: SecurityInput, params: Input) = {
      val timestamp = now()
      val (name, value) = FrameworkHeaders.`X-Request-Started-At`(timestamp)
      e.toReq(securityParams, params, timestamp)
        .uri
        // Add the client tracing as a query parameter because SSE requests do not support custom headers
        .addParam(name, value)
    }

    reconnectOnError match {
      case None =>
        val uri = createUri(securityParams, params)
        EventStream.fromDomEventSource(create(uri))

      case Some(onError) =>
        def stream(securityParams: SecurityInput, params: Input, index: Int): EventStream[MessageEvent] = {
          val uri = createUri(securityParams, params)

          EventStream.fromDomEventSourceEither(create(uri)).flatMapSwitch {
            case Right(value) =>
              onError.onMessage(value)
              EventStream.fromValue(value)

            case Left(error) =>
              val (newSecurityParams, newParams, wait) =
                onError.onError(uri, error, index, securityParams, params)(using definedAt)
              EventStream.delay(wait.toMillis.toInt).flatMapSwitch(_ => stream(newSecurityParams, newParams, index + 1))
          }
        }

        stream(securityParams, params, 0)
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
    decode: String => Either[String, Output],
    now: () => FrameworkDateTime = FrameworkDateTime.now,
    reconnectOnError: Option[OnSSEStreamError[SecurityInput, Input, (MessageEvent, Output)]] = Some(
      OnSSEStreamError.default[SecurityInput, Input, (MessageEvent, Output)]
    ),
    withCredentials: Boolean = false,
  )(using
    baseUri: AppBaseUri,
    definedAt: DefinedAt,
  ): EventStream[(MessageEvent, Output)] = {
    val modifiedReconnectOnError = reconnectOnError.map { original =>
      original.withOnMessage[MessageEvent] { _ =>
        // Do nothing here, we are going to invoke the handler on the original after decoding the message.
      }
    }

    val evtStream = toSSEStreamRaw(securityParams, params, now, modifiedReconnectOnError, withCredentials)
    evtStream.map { evt =>
      val either = for {
        dataStr <- evt.data match {
          case str: String => Right(str)
          case other       => Left(s"Event data is not a string: $other")
        }
        output <- decode(dataStr).left.map(err => s"Failed to decode event data '$dataStr': $err")
      } yield (evt, output)

      either match {
        case Left(err) => throw new Exception(err)
        case Right(value) =>
          reconnectOnError match {
            case None          =>
            case Some(handler) => handler.onMessage(value)
          }

          value
      }
    }
  }

  /** Turns the endpoint into a server-sent event stream, decoding the messages as JSON.
    *
    * Decoding failures will throw an exception.
    *
    * @param withCredentials
    *   A boolean value, defaulting to false, indicating if CORS should be set to include credentials.
    */
  def toSSEStreamJson(
    securityParams: SecurityInput,
    params: Input,
    now: () => FrameworkDateTime = FrameworkDateTime.now,
    reconnectOnError: Option[OnSSEStreamError[SecurityInput, Input, (MessageEvent, Output)]] = Some(
      OnSSEStreamError.default[SecurityInput, Input, (MessageEvent, Output)]
    ),
    withCredentials: Boolean = false,
  )(using
    baseUri: AppBaseUri,
    codec: CirceDecoder[Output],
    definedAt: DefinedAt,
  ): EventStream[(MessageEvent, Output)] = {
    toSSEStream(
      securityParams,
      params,
      codec.parseAndDecode(_).left.map(err => s"Failed to decode event data: $err"),
      now,
      reconnectOnError,
      withCredentials,
    )
  }
}
