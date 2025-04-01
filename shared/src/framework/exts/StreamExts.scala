package framework.exts

import fs2.Chunk
import cats.effect.Concurrent

extension [F[_], A](stream: fs2.Stream[F, A]) {

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

extension [F[_], A](iter: IterableOnce[fs2.Stream[F, A]]) {

  /** Merges all streams in the iterator into a single stream. */
  def mergeAll(using Concurrent[F]): fs2.Stream[F, A] =
    iter.iterator.foldLeft(fs2.Stream.empty: fs2.Stream[F, A])(_.merge(_))
}
