package framework.utils

import cats.effect.IO
import cats.effect.kernel.{Deferred, Resource}
import framework.utils.StreamRegistry.{StreamCount, StreamName, StreamStats}
import fs2.Stream

import java.util.concurrent.CancellationException

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
      } yield (getResult, fiber.cancel, deferred, registry)
    )
  )

  fixture.test("register - normal completion") { case (getResult, cancel, deferred, registry) =>
    for {
      _ <- assertIO(registry.get, Map(streamName -> StreamStats(StreamCount(1), Set.empty)))
      _ <- deferred.complete(Right(1))
      result <- getResult
      _ <- assertIO(registry.get, Map.empty)
      _ <- IO(assertEquals(result, Vector(1)))
    } yield ()
  }

  fixture.test("register - error completion") { case (getResult, cancel, deferred, registry) =>
    for {
      _ <- assertIO(registry.get, Map(streamName -> StreamStats(StreamCount(1), Set.empty)))
      exception = new Exception("test")
      _ <- deferred.complete(Left(exception))
      result <- getResult.attempt
      _ <- assertIO(registry.get, Map.empty)
      _ <- IO(assertEquals(result, Left(exception)))
    } yield ()
  }

  fixture.test("register - cancellation") { case (getResult, cancel, deferred, registry) =>
    for {
      _ <- assertIO(registry.get, Map(streamName -> StreamStats(StreamCount(1), Set.empty)))
      _ <- cancel
      _ <- interceptMessageIO[CancellationException]("Outcome was Canceled")(getResult)
      _ <- assertIO(registry.get, Map.empty)
    } yield ()
  }
}
