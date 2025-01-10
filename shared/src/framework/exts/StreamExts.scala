package framework.exts

import fs2.Chunk

extension [F[_], A](stream: fs2.Stream[F, A]) {

  /** Evaluates the given function on each chunk of the stream. */
  def evalTapChunks[X](f: Chunk[A] => F[X]): fs2.Stream[F, A] =
    stream.chunks.evalTap(f).unchunks
}
