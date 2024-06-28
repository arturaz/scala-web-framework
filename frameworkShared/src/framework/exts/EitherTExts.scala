package framework.exts

import cats.data.EitherT
import cats.MonadError
import cats.syntax.flatMap.*

extension [F[_], A, B](eitherT: EitherT[F, A, B]) {

  /** Get the value from the [[EitherT]], or raise an error by transforming the value in the [[Left]] side into an
    * error.
    */
  def getOrRaise2[E](e: A => E)(using m: MonadError[F, ? >: E]): F[B] = {
    eitherT.value.flatMap {
      case Left(err) => m.raiseError(e(err))
      case Right(v)  => m.pure(v)
    }
  }
}
