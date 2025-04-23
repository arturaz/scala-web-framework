package framework.exts

import alleycats.Empty
import cats.effect.IO
import com.raquo.airstream.core.{Observer, Signal}
import com.raquo.airstream.misc.StreamFromSignal
import com.raquo.airstream.ownership.ManualOwner

import scala.collection.immutable.Queue
import scala.concurrent.Future

trait SignalExts {
  extension [A](signal: Signal[A]) {

    /** Subscribes to a [[Signal]] to receive the first value and immediately unsubscribes. */
    def toIO: IO[A] =
      signal.toIOMapFilter(Some(_))

    /** Subscribes to a [[Signal]] to receive the first value that matches the predicate and immediately unsubscribes.
      */
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

    /** Ensures that the [[Future]]s are executed in the order they are received. Even if you push many events to the
      * source signal they will be processed one by one.
      *
      * The value is [[None]] until the first [[Future]] completes. If [[Future]] fails the error is propagated to the
      * output signal but the processing does not stop.
      */
    def sequentially[B](f: A => Future[B]): Signal[Option[B]] = {
      val lastEvt = Var(Option.empty[B])

      sealed trait State derives CanEqual
      case object Idle extends State
      case class Running(queue: Queue[A]) extends State {
        def enqueue(a: A): Running = copy(queue = queue.enqueue(a))

        def tryDequeue(): (Option[(A, Running)]) = {
          if (queue.isEmpty) None
          else Some((queue.head, copy(queue = queue.tail)))
        }
      }
      var current: State = Idle

      def pullNextFromQueue(): Unit = {
        current match {
          case v: Running =>
            v.tryDequeue() match {
              case None =>
                current = Idle
              case Some((a, next)) =>
                current = next
                launch(a)
            }

          case Idle =>
          // do nothing
        }
      }

      def launch(a: A): Unit = {
        val future = f(a)
        future.onComplete {
          case util.Failure(err) =>
            lastEvt.writer.onError(err)
            pullNextFromQueue()

          case util.Success(b) =>
            lastEvt.set(Some(b))
            pullNextFromQueue()
        }
      }

      def onSourceValue(a: A): Unit = {
        current = current match {
          case v: Idle.type =>
            launch(a)
            Running(Queue.empty)

          case v: Running =>
            v.enqueue(a)
        }
      }

      signal.flatMapSwitch { a =>
        onSourceValue(a)
        lastEvt.signal
      }
    }

    def sequentially[B](f: A => Future[B], initial: => B): Signal[B] = {
      signal.sequentially(f).map(_.getOrElse(initial))
    }
  }
}

given frameworkSignalExts: SignalExts = new SignalExts {}

trait SignalOptionExts {
  extension [A](signal: Signal[Option[A]]) {

    /** Map the [[Some]] value in the signal using `Option.flatMap` */
    def mapSomeOpt[B](f: A => Option[B]): Signal[Option[B]] =
      signal.map(_.flatMap(f))

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
}

given frameworkSignalOptionExts: SignalOptionExts = new SignalOptionExts {}
