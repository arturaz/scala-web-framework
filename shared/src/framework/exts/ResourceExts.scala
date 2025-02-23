package framework.exts

import cats.data.ValidatedNec
import cats.effect.kernel.Resource
import cats.effect.syntax.all.*
import cats.syntax.all.*
import cats.{ApplicativeError, Show}

extension [F[_], A](resource: Resource[F, A]) {

  /** As `attempt` but returns [[ValidatedNec]] instead of [[Either]]. */
  def attemptNec[E](using ApplicativeError[F, E]): Resource[F, ValidatedNec[E, A]] =
    resource.attempt.map(_.toValidatedNec)

  /** As `attempt` but returns [[ValidatedNec]] of [[String]] instead of [[Either]]. */
  def attemptNecAsString[E](prefix: String)(using ApplicativeError[F, E])(using
    Show[E]
  ): Resource[F, ValidatedNec[String, A]] =
    attemptNec.map(_.leftMap(errs => errs.map(err => show"$prefix: $err")))
}
