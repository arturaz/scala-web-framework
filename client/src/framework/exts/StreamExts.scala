package framework.exts

import cats.effect.kernel.{Async, Deferred}
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import fs2.Stream
import org.scalajs.dom.{ReadableStream, ReadableStreamType, ReadableStreamUnderlyingSource}

import scala.scalajs.js.JavaScriptException
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.chaining.*

extension [F[_]](stream: Stream[F, Byte]) {

  /** Converts a FS2 [[Stream]] to a JS [[ReadableStream]].
    *
    * @note
    *   Firefox doesn't support using streams with `fetch` requests.
    *   - https://developer.chrome.com/docs/capabilities/web-apis/fetch-streaming-requests
    *   - https://developer.mozilla.org/en-US/docs/Web/API/Request/body#browser_compatibility
    */
  def toJS(using F: Async[F]): Resource[F, ReadableStream[Uint8Array]] = {
    fs2.dom.toReadableStreamResource(stream)
  }
}
