package framework.utils

import cats.effect.kernel.Temporal
import cats.effect.std.Supervisor
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import framework.data.StreamWithInitializerData
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
  def subscribeTo[F[_]: Temporal, V](stream: Stream[F, V], waitForSubscription: FiniteDuration = 200.millis)(using
    supervisor: Supervisor[F]
  ): F[F[Vector[V]]] = {
    for {
      events <- Ref[F].of(Vector.empty[V])
      readEvents = stream.evalTap(evt => events.update(_ :+ evt)).compile.drain
      _ <- supervisor.supervise(readEvents)
      _ <- Temporal[F].sleep(waitForSubscription)
    } yield events.get
  }

  extension [InitData, Event](stream: Stream[IO, StreamWithInitializerData[InitData, Event]]) {
    def waitForInitializerData: IO[InitData] =
      stream.collectFirst { case StreamWithInitializerData.Initialize(data) => data }.compile.lastOrError
  }
}
