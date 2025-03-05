package framework.utils

import cats.effect.std.Supervisor
import cats.effect.{IO, Ref, Resource}
import fs2.Stream

import scala.concurrent.duration.*

trait FrameworkServerTestSuiteHelpers {

  /** A supervisor to use for a single test. Kills the fibers when the test finishes. */
  def testSupervisor: Resource[IO, Supervisor[IO]] =
    Supervisor[IO](await = false)

  /** Subscribes to a channel in background and returns a way to get events that we have received so far.
    *
    * @param waitForSubscription
    *   how long to wait for the subscription to establish. Unfortunately we have no better way to know when it has
    *   started except to wait a bit :/
    *
    * @param supervisor
    *   a supervisor to run the subscription in. Normally this is provided by the test case.
    */
  def subscribeTo[V](stream: Stream[IO, V], waitForSubscription: FiniteDuration = 200.millis)(using
    supervisor: Supervisor[IO]
  ): IO[IO[Vector[V]]] = {
    for {
      events <- Ref[IO].of(Vector.empty[V])
      readEvents = stream.evalTap(evt => events.update(_ :+ evt)).compile.drain
      _ <- supervisor.supervise(readEvents)
      _ <- IO.sleep(waitForSubscription)
    } yield events.get
  }
}
