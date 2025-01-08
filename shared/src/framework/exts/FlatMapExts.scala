package framework.exts

import cats.data.EitherT
import cats.syntax.flatMap.*
import cats.{FlatMap, Monad}

extension [F[_], A](fa: F[A]) {

  /** Performs a `flatMap` lifting into [[EitherT]]. */
  def flatMapT[L, B](f: A => EitherT[F, L, B])(using FlatMap[F]): EitherT[F, L, B] =
    EitherT(fa.flatMap(f(_).value))

  /** Performs a `flatten` lifting into [[EitherT]].
    *
    * Useful to have nice expressions when you have an, for example, `IO` and then it becomes `EitherT[IO, Err, A]` at
    * the end.
    *
    * Example:
    * {{{
    * (for {
    *   auth <- authSignal.toIO.map(_.client.authData)
    *   _ <- IO(lastClickedButton.set(submission.map(_.response)))
    *   reqId <- reqIdIO
    * } yield Endpoints.DrugRequests.submitResponse
    *   .toReq(auth, SubmitPharmacyDrugRequestResponseRequest(reqId, submission))
    *   .io // EitherT[IO, NetworkError, SubmitPharmacyDrugRequestResponseResponse]
    * ).flattenT
    * }}}
    */
  def flattenT[L, B](using FlatMap[F], A =:= EitherT[F, L, B]): EitherT[F, L, B] =
    EitherT(fa.flatMap(_.value))

  /** Runs `fa` just for the side-effect of it and then runs `that`. */
  infix def *>[A1, B](that: EitherT[F, A1, B])(using Monad[F]): EitherT[F, A1, B] =
    EitherT.right(fa).flatMap(_ => that)
}
