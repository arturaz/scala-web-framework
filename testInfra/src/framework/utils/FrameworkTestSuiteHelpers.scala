package framework.utils

import cats.effect.kernel.Ref
import cats.effect.{IO, SyncIO}
import cats.{Applicative, Functor, Semigroupal}
import munit.{CatsEffectSuite, Compare, Location, TestOptions}

import scala.annotation.targetName
import scala.concurrent.Future
import scala.util.Try

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
              // Save the value that we received for teardown.
              lastA = Some(a)
              f(a)
            }(munitExecutionContext),
        teardown = _ => fixture.teardown(lastA.get),
      )
    }

    def mapAsyncIO[B](f: A => IO[B]): FunFixture[B] =
      fixture.mapAsync(a => f(a).unsafeToFuture())

    /** Joins the two fixtures into one, using the provided function to combine the values. */
    def and[B, C](other: FunFixture[B])(combine: (A, B) => C): FunFixture[C] = {
      FunFixture
        .async(
          setup = opts => {
            val aFuture = fixture.setup(opts)
            val bFuture = other.setup(opts)
            aFuture.transform(util.Success(_)).zip(bFuture.transform(util.Success(_)))
          },
          teardown = { case (aTry, bTry) =>
            val aTeardown = aTry.fold(_ => Future.unit, fixture.teardown)
            val bTeardown = bTry.fold(_ => Future.unit, other.teardown)
            aTeardown.zipWith(bTeardown)((_, _) => ())
          },
        )
        .map { case (aTry, bTry) =>
          combine(aTry.get, bTry.get)
        }
    }
  }

  extension [T](fixture: SyncIO[FunFixture[T]]) {

    /** Runs the test providing the [[T]] as the given value. */
    def testGiven(name: String)(
      body: T ?=> Any
    )(using Location): Unit = {
      fixture.unsafeRunSync().test(TestOptions(name))(body(using _))
    }

    /** Runs the test providing the [[T]] as the given value. */
    def testGiven(options: TestOptions)(
      body: T ?=> Any
    )(using Location): Unit = {
      fixture.unsafeRunSync().test(options)(body(using _))
    }
  }

  extension [A](a: A) {
    infix def shouldBe[B](b: B)(using Location, Compare[A, B]): Unit =
      assertEquals(obtained = a, expected = b)
  }
}
