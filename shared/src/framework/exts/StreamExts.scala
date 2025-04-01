package framework.exts

import fs2.{Chunk, Stream}

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
}
