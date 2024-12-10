package framework.utils

import munit.{CatsEffectSuite, Compare, Location}

import scala.concurrent.Future
import cats.effect.IO
import scala.annotation.targetName

trait FrameworkTestSuiteHelpers { self: CatsEffectSuite =>
  extension [A](fixture: FunFixture[A]) {
    def map[B](f: A => B): FunFixture[B] =
      mapAsync(a => Future.successful(f(a)))

    @targetName("mapAsyncFuture")
    def mapAsync[B](f: A => Future[B]): FunFixture[B] = {
      var lastA = Option.empty[A]

      FunFixture.async[B](
        setup = opts =>
          fixture
            .setup(opts)
            .flatMap { a =>
              lastA = Some(a)
              f(a)
            }(munitExecutionContext),
        teardown = _ => fixture.teardown(lastA.get),
      )
    }

    def mapAsyncIO[B](f: A => IO[B]): FunFixture[B] =
      fixture.mapAsync(a => f(a).unsafeToFuture())
  }

  extension [A](a: A) {
    infix def shouldBe[B](b: B)(using Location, Compare[A, B]) =
      assertEquals(obtained = a, expected = b)
  }
}
