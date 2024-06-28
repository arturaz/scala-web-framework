package framework.exts

import cats.data.EitherT
import cats.FlatMap
import cats.syntax.flatMap.*

extension [F[_], A](fa: F[A]) {

  /** Performs a `flatMap` lifting into [[EitherT]]. */
  def flatMapT[L, B](f: A => EitherT[F, L, B])(using FlatMap[F]): EitherT[F, L, B] =
    EitherT(fa.flatMap(f(_).value))
}
