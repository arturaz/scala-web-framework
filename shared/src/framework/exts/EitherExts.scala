package framework.exts

import cats.Show
import cats.syntax.show.*
import alleycats.Empty

extension [A](self: Either[Nothing, A]) {

  /** Safely gets the value out of [[Either]] as the [[Left]] side can not be instantiated. */
  def getSafe: A = self match {
    case Left(_)      => throw new Exception("impossible")
    case Right(value) => value
  }
}

implicit class EitherExts[A, B](private val e: Either[A, B]) extends AnyVal {

  /** Returns the value in the [[Right]] case or throws an exception in the [[Left]] case. */
  inline def getOrThrow: B = e match {
    case Left(value)  => throw new IllegalStateException(s"Expected Right, got Left($value)")
    case Right(value) => value
  }

  /** Returns the value in the [[Right]] case or the [[Empty]] value in the [[Left]] case. */
  def getOrEmpty(using empty: Empty[B]): B = e match {
    case Left(_)      => empty.empty
    case Right(value) => value
  }

  /** Converts the [[Left]] side to [[Exception]]. Useful for [[cats.effect.IO.fromEither]]. */
  def leftToException(using Show[A]): Either[Exception, B] =
    e.left.map(e => new Exception(show"Expected Right, got Left($e)"))
}
