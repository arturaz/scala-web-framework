package framework.api

import cats.{ApplicativeThrow, Functor}
import framework.prelude.*

/** Whether the request was successful or not with accompanying data if it was. */
case class RequestSuccessfulOf[+A](successValue: Option[A]) derives CanEqual, Schema, CirceCodec {
  def map[B](f: A => B): RequestSuccessfulOf[B] = RequestSuccessfulOf(successValue.map(f))

  def isSuccessful: Boolean = successValue.nonEmpty

  def withoutValue: RequestSuccessful = RequestSuccessful(isSuccessful)
}
object RequestSuccessfulOf {
  def successful[A](value: A): RequestSuccessfulOf[A] = apply(Some(value))

  val failed: RequestSuccessfulOf[Nothing] = apply(None)

  def fromBoolean[A](succesfull: Boolean, successValue: A): RequestSuccessfulOf[A] =
    if (succesfull) successful(successValue) else failed

  def fromExpected[SuccessValue, ComparisonValue: CanEqual1](value: SuccessValue, expected: ComparisonValue)(
    actual: ComparisonValue
  ): RequestSuccessfulOf[SuccessValue] =
    if (expected == actual) successful(value) else failed

  given [A]: Conversion[RequestSuccessfulOf[A], Boolean] = _.isSuccessful

  given functor: Functor[RequestSuccessfulOf] with {
    override def map[A, B](fa: RequestSuccessfulOf[A])(f: A => B): RequestSuccessfulOf[B] = fa.map(f)
  }

  extension [A](self: RequestSuccessfulOf[A]) {

    /** Raises an error if the request was not successful, otherwise returns the value. */
    def getOrRaiseOnFailure[F[_]: ApplicativeThrow](onError: => Throwable): F[A] = self.successValue match {
      case None        => ApplicativeThrow[F].raiseError(onError)
      case Some(value) => ApplicativeThrow[F].pure(value)
    }
  }
}
