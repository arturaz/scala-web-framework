package framework.utils

import cats.{Applicative, Functor, Semigroupal}
import cats.syntax.all.*
import munit.FunFixtures

trait FrameworkTestSuiteFunFixtureT { self: FunFixtures & FrameworkTestSuiteHelpers =>

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
}
