package framework.exts

import cats.effect.kernel.Async
import cats.syntax.all.*
import fs2.{Chunk, Stream}
import org.scalajs.dom.ReadableStream

import scala.scalajs.js.typedarray.Uint8Array

extension (stream: ReadableStream[Uint8Array]) {

  /** Converts a [[ReadableStream]] to an FS2 [[Stream]]. */
  def toFs2[F[_]](using a: Async[F]): Stream[F, Byte] =
    fs2.dom.readReadableStream(a.pure(stream))
}
