package framework.exts

import scala.concurrent.{ExecutionContext, Future}

extension [A](future: Future[A]) {

  /** Returns a [[Future]] that completes with `Right(value)` if the original future completes successfully, or
    * `Left(cause)` if the original future fails.
    */
  def attempt: Future[Either[Throwable, A]] = future.transformWith {
    case util.Success(value) => Future.successful(Right(value))
    case util.Failure(cause) => Future.successful(Left(cause))
  }(using ExecutionContext.parasitic)
}

extension [A](future: Future[Option[A]]) {

  /** Returns a [[Future]] that never completes if the original future completes with `None`, or completes with the
    * value inside the `Some`.
    */
  def orNever: Future[A] = future.flatMap {
    case None        => Future.never
    case Some(value) => Future.successful(value)
  }(using ExecutionContext.parasitic)
}
