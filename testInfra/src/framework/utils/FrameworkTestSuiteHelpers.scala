package framework.utils

import cats.effect.kernel.Ref
import cats.effect.{IO, SyncIO}
import cats.{Applicative, Functor, Semigroupal}
import munit.{CatsEffectSuite, Compare, Location, TestOptions}

import scala.annotation.targetName
import scala.concurrent.Future
import scala.util.Try

trait FrameworkTestSuiteHelpers { self: CatsEffectSuite =>
  export cats.syntax.all.*

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
    def testGiven(name: String)(
      body: T ?=> Any
    )(using Location): Unit = {
      fixture.unsafeRunSync().test(TestOptions(name))(body(using _))
    }

    def testGiven(options: TestOptions)(
      body: T ?=> Any
    )(using Location): Unit = {
      fixture.unsafeRunSync().test(options)(body(using _))
    }
  }

  given funFixtureApplicative: Applicative[FunFixture] with {
    override def pure[A](x: A): FunFixture[A] = FunFixture(setup = _ => x, teardown = _ => ())

    override def ap[A, B](ff: FunFixture[A => B])(fa: FunFixture[A]): FunFixture[B] =
      ff.and(fa)((fn, a) => fn(a))
  }

  /** Monad transformer for [[FunFixture]]. */
  case class FunFixtureT[F[_], A](value: F[FunFixture[A]]) {
    def map[B](f: A => B)(using Functor[F]): FunFixtureT[F, B] =
      FunFixtureT(value.map(_.map(f)))

    def and[B, C](other: FunFixtureT[F, B])(combine: (A, B) => C)(using Functor[F], Semigroupal[F]): FunFixtureT[F, C] =
      FunFixtureT((value, other.value).mapN((aFixture, bFixture) => aFixture.and(bFixture)(combine)))
  }
  object FunFixtureT {
    def pure[F[_]: Applicative, A](a: A): FunFixtureT[F, A] =
      apply(Applicative[F].pure(a.pure))

    given applicative[F[_]: Applicative]: Applicative[[X] =>> FunFixtureT[F, X]] with {
      override def pure[A](x: A): FunFixtureT[F, A] = FunFixtureT.pure(x)

      override def ap[A, B](ff: FunFixtureT[F, A => B])(fa: FunFixtureT[F, A]): FunFixtureT[F, B] =
        ff.and(fa)((fn, a) => fn(a))
    }
  }

  extension [A](a: A) {
    infix def shouldBe[B](b: B)(using Location, Compare[A, B]) =
      assertEquals(obtained = a, expected = b)
  }

  /** Generates values for use in tests. */
  trait TestValues {
    private val strings: Ref[SyncIO, Map[String, Long]] = Ref.unsafe(Map.empty)

    private def bumped[K](ref: Ref[SyncIO, Map[K, Long]], k: K): Long =
      ref
        .modify { map =>
          map.get(k) match {
            case None    => (map.updated(k, 0L), 0L)
            case Some(v) => (map.updated(k, v + 1L), v + 1L)
          }
        }
        .unsafeRunSync()

    /** Generates a unique string for use in tests. */
    def string(prefix: String): String = s"$prefix ${bumped(strings, prefix)}"
  }
  object TestValues extends TestValues
}
