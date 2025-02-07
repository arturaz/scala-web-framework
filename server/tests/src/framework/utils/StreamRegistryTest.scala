package framework.utils

import cats.effect.IO
import cats.effect.kernel.{Deferred, Resource}
import framework.utils.StreamRegistry.{StreamCount, StreamName, StreamStats}
import fs2.Stream

import java.util.concurrent.CancellationException
import framework.utils.StreamRegistry.StreamDetails

class StreamRegistryTest extends FrameworkTestSuite {
  val streamName = StreamName.makeOrThrow("test")

  val fixture = ResourceFunFixture(
    Resource.eval(
      for {
        startDeferred <- Deferred[IO, Unit]
        deferred <- Deferred[IO, Either[Throwable, Int]]
        getValue = deferred.get.rethrow
        registry <- StreamRegistry.withoutMetrics[IO]
        fiber <- registry
          .register(streamName, details = None, Stream.eval(startDeferred.complete(()) >> getValue))
          .compile
          .toVector
          .start
        getResult = fiber.join.flatMap(_.embedError)
        _ <- startDeferred.get
        get = registry.get.map(mapGet)
      } yield (getResult, fiber.cancel, deferred, get)
    )
  )

  case class StreamStatsTest(count: StreamCount, details: Set[StreamDetails])
  object StreamStatsTest {
    def fromStreamStats(s: StreamStats): StreamStatsTest =
      apply(s.count, s.details.valuesIterator.toSet)
  }

  def mapGet(m: Map[StreamName, StreamStats]): Map[StreamName, StreamStatsTest] =
    m.view.mapValues(StreamStatsTest.fromStreamStats).toMap

  fixture.test("register - normal completion") { case (getResult, cancel, deferred, get) =>
    for {
      _ <- IO(assertIO(get, Map(streamName -> StreamStatsTest(StreamCount(1), Set.empty))))
      _ <- deferred.complete(Right(1))
      result <- getResult
      _ <- assertIO(get, Map.empty)
      _ <- IO(assertEquals(result, Vector(1)))
    } yield ()
  }

  fixture.test("register - error completion") { case (getResult, cancel, deferred, get) =>
    for {
      _ <- assertIO(get, Map(streamName -> StreamStatsTest(StreamCount(1), Set.empty)))
      exception = new Exception("test")
      _ <- deferred.complete(Left(exception))
      result <- getResult.attempt
      _ <- assertIO(get, Map.empty)
      _ <- IO(assertEquals(result, Left(exception)))
    } yield ()
  }

  fixture.test("register - cancellation") { case (getResult, cancel, deferred, get) =>
    for {
      _ <- assertIO(get, Map(streamName -> StreamStatsTest(StreamCount(1), Set.empty)))
      _ <- cancel
      _ <- interceptMessageIO[CancellationException]("Outcome was Canceled")(getResult)
      _ <- assertIO(get, Map.empty)
    } yield ()
  }
}
