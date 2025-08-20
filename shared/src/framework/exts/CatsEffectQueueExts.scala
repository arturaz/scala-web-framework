package framework.exts

import cats.effect.std.{Queue, QueueSink, QueueSource}

extension [F[_], A](queue: Queue[F, A]) {
  def asSink: QueueSink[F, A] = queue
  def asSource: QueueSource[F, A] = queue
}
