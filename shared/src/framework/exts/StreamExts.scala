package framework.exts

import fs2.{Chunk, Stream}
import cats.Functor
import cats.syntax.all.*

trait StreamExts {
  extension [F[_], A](stream: Stream[F, A]) {

    /** Evaluates the given function on each chunk of the stream. */
    def evalTapChunks[X](f: Chunk[A] => F[X]): fs2.Stream[F, A] =
      stream.chunks.evalTap(f).unchunks

    /** Maps each chunk of the stream using the given function, only keeping the [[Some]] results. */
    def mapFilter[B](f: A => Option[B]): fs2.Stream[F, B] =
      stream.mapChunks(chunk => chunk.mapFilter(f))

    /** Alias for [[mapFilter]]. */
    inline def collectSome[B](f: A => Option[B]): fs2.Stream[F, B] =
      mapFilter(f)

    /** Evaluates the given function on each chunk of the stream, passing the total chunk size that has been accrued so
      * far and the chunk itself.
      */
    def evalTapChunksWithItemCount(f: (Long, Chunk[A]) => F[Unit])(using Functor[F]): fs2.Stream[F, A] =
      evalMapChunksWithItemCount((count, chunk) => f(count, chunk).as(chunk))

    /** Evaluates the given function on each chunk of the stream, passing the total chunk size that has been accrued so
      * far and the chunk itself.
      */
    def evalMapChunksWithItemCount[B](f: (Long, Chunk[A]) => F[Chunk[B]])(using Functor[F]): fs2.Stream[F, B] =
      stream.chunks
        .evalScan((0L, Chunk.empty[B])) { case ((totalCount, _), chunk) =>
          val newCount = totalCount + chunk.size
          f(newCount, chunk).map(b => (newCount, b))
        }
        .map(_._2)
        .unchunks
  }
}

given frameworkFs2StreamExts: StreamExts = new StreamExts {}
