package framework.exts

import cats.Show
import cats.data.ValidatedNec
import cats.effect.syntax.all.*
import cats.effect.{MonadCancelThrow, Resource}
import cats.syntax.all.*

extension [F[_], A](resource: Resource[F, A]) {

  /** As `attempt` but returns [[ValidatedNec]] instead of [[Either]]. */
  def attemptNec(using MonadCancelThrow[F]): Resource[F, ValidatedNec[Throwable, A]] =
    resource.attempt.map(_.toValidatedNec)

  /** As `attempt` but returns [[ValidatedNec]] of [[String]] instead of [[Either]]. */
  def attemptNecAsString(prefix: String)(using MonadCancelThrow[F])(using
    Show[Throwable]
  ): Resource[F, ValidatedNec[String, A]] =
    attemptNec.map(_.leftMap(errs => errs.map(err => show"$prefix: $err")))
}
