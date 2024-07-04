package framework.exts

import cats.data.EitherT
import cats.syntax.flatMap.*
import cats.{FlatMap, Monad}

extension [F[_], A](fa: F[A]) {

  /** Performs a `flatMap` lifting into [[EitherT]]. */
  def flatMapT[L, B](f: A => EitherT[F, L, B])(using FlatMap[F]): EitherT[F, L, B] =
    EitherT(fa.flatMap(f(_).value))

  /** Runs `fa` just for the side-effect of it and then runs `that`. */
  infix def *>[A1, B](that: EitherT[F, A1, B])(using Monad[F]): EitherT[F, A1, B] =
    EitherT.right(fa).flatMap(_ => that)
}
