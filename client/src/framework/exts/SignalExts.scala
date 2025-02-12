package framework.exts

import alleycats.Empty
import cats.effect.IO
import com.raquo.airstream.core.{Observer, Signal}
import com.raquo.airstream.misc.StreamFromSignal
import com.raquo.airstream.ownership.ManualOwner

extension [A](signal: Signal[A]) {

  /** Subscribes to a [[Signal]] to receive the first value and immediately unsubscribes. */
  def toIO: IO[A] =
    signal.toIOMapFilter(Some(_))

  /** Subscribes to a [[Signal]] to receive the first value that matches the predicate and immediately unsubscribes. */
  def toIOMapFilter[B](f: A => Option[B]): IO[B] = {
    IO.async[B] { callback =>
      IO {
        val owner = new ManualOwner
        signal.addObserver(Observer(a => {
          f(a) match {
            case None =>
            // do nothing
            case Some(b) =>
              owner.killSubscriptions()
              callback(Right(b))
          }
        }))(owner)

        Some(IO(owner.killSubscriptions()))
      }
    }
  }

  /** Maps the value of the [[Signal]] passing the [[A]] as context. */
  def mapImplicit[B](f: A ?=> B): Signal[B] = signal.map(a => f(using a))

  // This one is a bit iffy, but hey, we have no better option...
  // https://github.com/raquo/Airstream/issues/124
  // https://github.com/raquo/Airstream/issues/132
  // https://discordapp.com/channels/1020225759610163220/1020225760075718669/1327296294959448229
  def toEventStream: EventStream[A] =
    EventStream.merge(EventStream.unit().sample(signal), signal.changes)

  /** Gives a window of the previous and the current value of the [[Signal]]. */
  def changedValues: Signal[(Option[A], A)] = {
    signal.scanLeft(a => (Option.empty[A], a)) { case ((_, previous), current) => (Some(previous), current) }
  }
}

extension [A](signal: Signal[Option[A]]) {

  /** Subscribes to a [[Signal]] to receive the first `Some` value and immediately unsubscribes. */
  def toIOOnSome: IO[A] =
    signal.toIOMapFilter(identity)

  /** Subscribes to a [[Signal]] to receive the first `None` value and immediately unsubscribes. */
  def toIOOnNone: IO[Unit] =
    signal.toIOMapFilter { case None => Some(()); case Some(_) => None }

  /** Splits the [[Signal]] by the [[Option]] cases, returning the [[Empty]] value if the [[Option]] is [[None]]. */
  def splitOptionOrEmpty[B](f: (A, Signal[A]) => B)(using empty: Empty[B]): Signal[B] =
    signal.splitOption(f, empty.empty)
}
