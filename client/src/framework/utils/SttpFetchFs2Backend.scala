// package framework.utils

// import cats.effect.kernel.syntax.monadCancel._
// import cats.effect.kernel.Async
// import org.scalajs.dom.{BodyInit, Request => FetchRequest, Response => FetchResponse}
// import sttp.capabilities.WebSockets
// import sttp.client3.internal.{ConvertFromFuture, NoStreams}
// import sttp.client3.{AbstractFetchBackend, FetchOptions, SttpBackend}
// import sttp.ws.WebSocket

// import scala.concurrent.Future
// import scala.scalajs.js
// import sttp.client3.impl.cats.CatsMonadAsyncError
// import sttp.capabilities.fs2.Fs2Streams
// import sttp.capabilities.Streams
// import scala.scalajs.js.UndefOr
// import sttp.ws.WebSocketFrame.Data
// import sttp.ws.WebSocketFrame
// import fs2.{Pipe, Stream}
// import cats.syntax.all.*
// import cats.effect.kernel.Ref
// import cats.effect.Concurrent
// import sttp.ws.WebSocketClosed

// class SttpFetchFs2Backend[F[_]] private (
//   fetchOptions: FetchOptions,
//   customizeRequest: FetchRequest => FetchRequest,
// )(using F: AsyncJS[F])
//     extends AbstractFetchBackend[F, Fs2Streams[F], Fs2Streams[F] & WebSockets](
//       fetchOptions,
//       customizeRequest,
//       new CatsMonadAsyncError,
//     ) {
//   override val streams: Fs2Streams[F] = Fs2Streams.apply

//   override protected def addCancelTimeoutHook[T](result: F[T], cancel: () => Unit, cleanup: () => Unit): F[T] = {
//     val doCancel = Async[F].delay(cancel())
//     val doCleanup = Async[F].delay(cleanup())
//     result.onCancel(doCancel).guarantee(doCleanup)
//   }

//   override protected def handleStreamBody(
//     stream: Stream[F, Byte]
//   ): F[UndefOr[BodyInit]] = {
//     stream.toJS.map(stream => stream: BodyInit)
//   }

//   override protected def handleResponseAsStream(
//     response: FetchResponse
//   ): F[(Stream[F, Byte], () => F[Unit])] =
//     F.delay {
//       val stream = response.body.toFs2
//       // Streams have cancellation built-in, so this isn't necessary
//       val cancel = () => F.unit
//       (stream, cancel)
//     }

//   override protected def compileWebSocketPipe(
//     ws: WebSocket[F],
//     pipe: fs2.Pipe[F, Data[?], WebSocketFrame],
//   ): F[Unit] = Fs2WebSockets.handleThroughPipe(ws)(pipe)

//   override def convertFromFuture: ConvertFromFuture[F] = new ConvertFromFuture[F] {
//     override def apply[T](f: Future[T]): F[T] = Async[F].fromFuture(responseMonad.unit(f))
//   }
// }

// object SttpFetchFs2Backend {
//   def apply[F[_]: AsyncJS](
//     fetchOptions: FetchOptions = FetchOptions.Default,
//     customizeRequest: FetchRequest => FetchRequest = identity,
//   ): SttpBackend[F, Fs2Streams[F] & WebSockets] = {
//     new SttpFetchFs2Backend(fetchOptions, customizeRequest)
//   }
// }

// // Copied from https://github.com/softwaremill/sttp/blob/master/effects/fs2-ce2/src/main/scala/sttp/client4/impl/fs2/Fs2WebSockets.scala
// object Fs2WebSockets {

//   /** Handle the websocket through a [[Pipe]] which receives the incoming events and produces the messages to be sent to
//     * the server. Not that by the nature of a [[Pipe]], there no need that these two streams are coupled. Just make sure
//     * to consume the input as otherwise the receiving buffer might overflow (use [[Stream.drain]] if you want to
//     * discard).
//     * @param ws
//     *   the websocket to handle
//     * @param pipe
//     *   the pipe to handle the socket
//     * @tparam F
//     *   the effect type
//     * @return
//     *   an Unit effect describing the full run of the websocket through the pipe
//     */
//   def handleThroughPipe[F[_]: Concurrent](
//     ws: WebSocket[F]
//   )(pipe: Pipe[F, WebSocketFrame.Data[?], WebSocketFrame]): F[Unit] =
//     Stream
//       .eval(Ref.of[F, Option[WebSocketFrame.Close]](None))
//       .flatMap { closeRef =>
//         Stream
//           .repeatEval(ws.receive()) // read incoming messages
//           .flatMap[F, Option[WebSocketFrame.Data[?]]] {
//             case WebSocketFrame.Close(code, reason) =>
//               Stream.eval(closeRef.set(Some(WebSocketFrame.Close(code, reason)))).as(None)
//             case WebSocketFrame.Ping(payload) =>
//               Stream.eval(ws.send(WebSocketFrame.Pong(payload))).drain
//             case WebSocketFrame.Pong(_) =>
//               Stream.empty // ignore
//             case in: WebSocketFrame.Data[_] => Stream.emit(Some(in))
//           }
//           .handleErrorWith {
//             case _: WebSocketClosed => Stream.eval(closeRef.set(None)).as(None)
//             case e                  => Stream.eval(Concurrent[F].raiseError(e))
//           }
//           .unNoneTerminate // terminate once we got a Close
//           .through(pipe)
//           // end with matching Close or user-provided Close or no Close at all
//           .append(Stream.eval(closeRef.get).unNone) // A Close isn't a continuation
//           .evalMap(ws.send(_)) // send messages
//       }
//       .compile
//       .drain
//       .guarantee(ws.close())

//   def fromTextPipe[F[_]]: (String => WebSocketFrame) => fs2.Pipe[F, WebSocketFrame, WebSocketFrame] =
//     f => fromTextPipeF(_.map(f))

//   def fromTextPipeF[F[_]]: fs2.Pipe[F, String, WebSocketFrame] => fs2.Pipe[F, WebSocketFrame, WebSocketFrame] =
//     p => p.compose(combinedTextFrames)

//   def combinedTextFrames[F[_]]: fs2.Pipe[F, WebSocketFrame, String] = { input =>
//     input
//       .collect { case tf: WebSocketFrame.Text => tf }
//       .flatMap { tf =>
//         if (tf.finalFragment) {
//           Stream(tf.copy(finalFragment = false), tf.copy(payload = ""))
//         } else {
//           Stream(tf)
//         }
//       }
//       .split(_.finalFragment)
//       .map(chunks => chunks.map(_.payload).toList.mkString)
//   }
// }
