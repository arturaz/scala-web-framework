package framework.exts

import scala.concurrent.{ExecutionContext, Future}

extension [A](future: Future[A]) {

  /** Returns a [[Future]] that completes with `Right(value)` if the original future completes successfully, or
    * `Left(cause)` if the original future fails.
    */
  def attempt: Future[Either[Throwable, A]] = future.transformWith {
    case util.Success(value) => Future.successful(Right(value))
    case util.Failure(cause) => Future.successful(Left(cause))
  }(ExecutionContext.parasitic)
}
