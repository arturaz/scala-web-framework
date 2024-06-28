package framework.exts

import cats.effect.IO
import scala.util.Try
import cats.syntax.functor.*
import cats.syntax.monadError.*
import cats.MonadError

extension [F[_], A](fa: F[Try[A]])(using MonadError[F, Throwable]) {

  /** As `rethrow` but for [[Try]] values. */
  def rethrowTry: F[A] = fa.map(_.toEither).rethrow
}
