package framework.exts

import com.raquo.airstream.core.Observable
import com.raquo.airstream.ownership.ManualOwner
import com.raquo.airstream.core.Observer

trait ObservableExts {
  extension [A](observable: Observable[A]) {

    /** Subscribes to a [[Observable]] to receive the first value and immediately unsubscribes. */
    def toIO: IO[A] =
      observable.toIOMapFilter(Some(_))

    /** Subscribes to a [[Observable]] to receive the first value that matches the predicate and immediately
      * unsubscribes.
      */
    def toIOMapFilter[B](f: A => Option[B]): IO[B] = {
      IO.async[B] { callback =>
        IO {
          val owner = new ManualOwner
          observable.addObserver(Observer(a => {
            f(a) match {
              case None =>
              // do nothing
              case Some(b) =>
                owner.killSubscriptions()
                callback(Right(b))
            }
          }))(using owner)

          Some(IO(owner.killSubscriptions()))
        }
      }
    }
  }
}
given frameworkObservableExts: ObservableExts = new ObservableExts {}
