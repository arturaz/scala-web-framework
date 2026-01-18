package framework.utils

import com.raquo.airstream.custom.CustomSource
import framework.api.FrameworkHeaders
import framework.data.EndpointSSEWithWS.ClientConnectionMode
import framework.data.{AppBaseUri, EndpointSSEWithWS, FrameworkDateTime}
import framework.prelude.sttpClientInterpreter
import framework.sourcecode.DefinedAt
import framework.tapir.capabilities.ServerSentEvents
import framework.utils.{NetworkError, NetworkOrAuthError, PrettyPrintDuration}
import org.scalajs.dom.{Event, EventSource, EventSourceInit, MessageEvent, WebSocket}
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.Request
import sttp.model.Uri
import sttp.tapir.{DecodeResult, Endpoint, PublicEndpoint}

import scala.annotation.targetName
import scala.concurrent.duration.*
import scala.scalajs.js.JSON
import framework.exts.OnSSEStreamError.OnErrorAction

trait ToSSEStreamBuilder[SecurityInput, Input, Output] {

  /** Implementation helper for [[raw]]. */
  protected def rawImpl(
    securityParams: SecurityInput,
    params: Input,
    reconnectOnError: Option[OnSSEStreamError[SecurityInput, Input, MessageEvent]] = Some(
      OnSSEStreamError.default[SecurityInput, Input, MessageEvent]
    ),
    createUri: (SecurityInput, Input) => Uri,
    createEventStream: Uri => EventStream[MessageEvent],
    createEithersEventStream: Uri => EventStream[Either[Event, MessageEvent]],
  )(using baseUri: AppBaseUri, definedAt: DefinedAt): EventStream[MessageEvent] = {
    reconnectOnError match {
      case None =>
        val uri = createUri(securityParams, params)
        createEventStream(uri)

      case Some(onError) =>
        def stream(securityParams: SecurityInput, params: Input, index: Int): EventStream[MessageEvent] = {
          val uri = createUri(securityParams, params)

          createEithersEventStream(uri).flatMapSwitch {
            case Right(value) =>
              onError.onMessage(value)
              EventStream.fromValue(value)

            case Left(error) =>
              onError.onError(uri, error, index, securityParams, params)(using definedAt) match {
                case OnSSEStreamError.OnErrorAction.ReconnectAfter(newSecurityParams, newParams, wait) =>
                  EventStream
                    .delay(wait.toMillis.toInt)
                    .flatMapSwitch(_ => stream(newSecurityParams, newParams, index + 1))

                case OnSSEStreamError.OnErrorAction.ReconnectWith(stream) =>
                  stream

                case OnSSEStreamError.OnErrorAction.Stop =>
                  EventStream.empty
              }

          }
        }

        stream(securityParams, params, 0)
    }
  }

  /** Turns the endpoint into a server-sent event stream without decoding.
    *
    * @param reconnectOnError
    *   If [[Some]] then when an error occurs the stream will be reconnected.
    * @param withCredentials
    *   A boolean value, defaulting to false, indicating if CORS should be set to include credentials.
    */
  def raw(
    securityParams: SecurityInput,
    params: Input,
    now: () => FrameworkDateTime = FrameworkDateTime.now,
    reconnectOnError: Option[OnSSEStreamError[SecurityInput, Input, MessageEvent]] = Some(
      OnSSEStreamError.default[SecurityInput, Input, MessageEvent]
    ),
    withCredentials: Boolean = false,
  )(using baseUri: AppBaseUri, definedAt: DefinedAt): EventStream[MessageEvent]

  /** Turns the endpoint into a server-sent event stream.
    *
    * Decoding failures will throw an exception.
    *
    * @param withCredentials
    *   A boolean value, defaulting to false, indicating if CORS should be set to include credentials.
    */
  def apply(
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

    val evtStream = raw(securityParams, params, now, modifiedReconnectOnError, withCredentials)
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
  def json(
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
    apply(
      securityParams,
      params,
      codec.parseAndDecode(_).left.map(err => s"Failed to decode event data: $err"),
      now,
      reconnectOnError,
      withCredentials,
    )
  }
}
/** Callbacks for SSE/WebSocket fallback lifecycle events. */
trait SSEFallbackCallbacks {

  /** Called when SSE fails and we switch to WebSocket. */
  def onFallbackToWebSocket(): Unit

  /** Called when SSE connection succeeds (first message received). */
  def onSSESuccess(): Unit
}
object SSEFallbackCallbacks {
  val noOp: SSEFallbackCallbacks = new SSEFallbackCallbacks {
    def onFallbackToWebSocket(): Unit = ()
    def onSSESuccess(): Unit = ()
  }

  /** Creates callbacks that persist the fallback state to a [[PersistedVar]].
    *
    * @param persistedVar
    *   The persisted var to update when fallback occurs or SSE succeeds.
    * @param recoveryTimeout
    *   How long to stay on WebSocket before trying SSE again.
    * @param logger
    *   Logger for logging fallback events. Use [[JSLogger.noOp]] to disable logging.
    */
  def persisted(
    persistedVar: PersistedVar[EndpointSSEWithWS.ClientConnectionMode],
    recoveryTimeout: FiniteDuration = EndpointSSEWithWS.ClientConnectionMode.defaultRecoveryTimeout,
    logger: JSLogger = JSLogger.noOp,
  ): SSEFallbackCallbacks = new SSEFallbackCallbacks {
    def onFallbackToWebSocket(): Unit = {
      persistedVar.underlying.now() match {
        case EndpointSSEWithWS.ClientConnectionMode.SSEWithWebSocketFallback(_) =>
          // Set forced WS mode for the recovery timeout
          val tryAgainAt = FrameworkDateTime.now() + recoveryTimeout
          logger.info(show"SSE failed, falling back to WebSocket. Will try SSE again at $tryAgainAt")
          persistedVar.setAndPersist(
            EndpointSSEWithWS.ClientConnectionMode.SSEWithWebSocketFallback(Some(tryAgainAt))
          )
        case EndpointSSEWithWS.ClientConnectionMode.ServerSentEvents
            | EndpointSSEWithWS.ClientConnectionMode.WebSocket =>
          // Manual override, don't change
          ()
      }
    }

    def onSSESuccess(): Unit = {
      persistedVar.underlying.now() match {
        case EndpointSSEWithWS.ClientConnectionMode.SSEWithWebSocketFallback(Some(_)) =>
          // SSE works again, clear the timestamp
          logger.info("SSE connection succeeded, clearing forced WebSocket mode")
          persistedVar.setAndPersist(EndpointSSEWithWS.ClientConnectionMode.SSEWithWebSocketFallback(None))
        case EndpointSSEWithWS.ClientConnectionMode.SSEWithWebSocketFallback(None)
            | EndpointSSEWithWS.ClientConnectionMode.ServerSentEvents
            | EndpointSSEWithWS.ClientConnectionMode.WebSocket =>
          // Already in normal mode or manual override, nothing to do
          ()
      }
    }
  }
}

object ToSSEStreamBuilder {

  /** Simple fallback handler to WebSockets for when establishing the SSE connection fails. */
  def sseWithWebSocketFallback[F[_], SecurityInput, Input, Output, AuthError, Requirements](
    e: EndpointSSEWithWS[F, SecurityInput, Input, Output, AuthError, Requirements],
    callbacks: SSEFallbackCallbacks,
    maxSseFailuresBeforeFallback: Int = 3,
    waitFor: Int => FiniteDuration = OnSSEStreamError.defaultWaitFor,
  ): ToSSEStreamBuilder[SecurityInput, Input, Output] = new {
    // Track whether we've notified SSE success for this connection
    private var sseSuccessNotified = false

    override def raw(
      securityParams: SecurityInput,
      params: Input,
      now: () => FrameworkDateTime,
      reconnectOnError: Option[OnSSEStreamError[SecurityInput, Input, MessageEvent]],
      withCredentials: Boolean,
    )(using baseUri: AppBaseUri, definedAt: DefinedAt): EventStream[MessageEvent] = {
      val sseReconnectOnError = new OnSSEStreamError[SecurityInput, Input, MessageEvent] {
        override def onMessage(msg: MessageEvent): Unit = {
          // First successful SSE message - notify callback
          if (!sseSuccessNotified) {
            sseSuccessNotified = true
            callbacks.onSSESuccess()
          }
          reconnectOnError.foreach(_.onMessage(msg))
        }

        override def onError(
          uri: Uri,
          error: Event,
          connectionIndex: Int,
          securityParams: SecurityInput,
          params: Input,
        )(using
          DefinedAt
        ): OnErrorAction[SecurityInput, Input] = {
          // connectionIndex: first failure is 0, then 1, ...
          val failuresSoFar = connectionIndex + 1

          val delay = waitFor(connectionIndex)

          def delayedWsStream = {
            // Notify fallback when switching to WS
            callbacks.onFallbackToWebSocket()
            EventStream
              .delay(delay.toMillis.toIntClamped)
              .flatMapSwitch(_ =>
                e.webSocketAsSSE.toWsAsSSEStream.raw(securityParams, params, now, reconnectOnError, withCredentials)
              )
          }

          reconnectOnError match {
            case None =>
              log.info(
                show"sseWithWebSocketFallback: reconnectOnError is None, reconnecting with WS in ${delay.prettyForDebug}"
              )
              OnSSEStreamError.OnErrorAction.ReconnectWith(delayedWsStream)
            case Some(_) if failuresSoFar >= maxSseFailuresBeforeFallback =>
              log.info(
                show"sseWithWebSocketFallback: failure at connection index $connectionIndex, " +
                  show"reconnecting with WS in ${delay.prettyForDebug}"
              )
              OnSSEStreamError.OnErrorAction.ReconnectWith(delayedWsStream)
            case Some(reconnectOnError) =>
              reconnectOnError.onError(uri, error, connectionIndex, securityParams, params)
          }
        }
      }

      e.sse.toSSEStream.raw(securityParams, params, now, Some(sseReconnectOnError), withCredentials = withCredentials)
    }
  }
}
