package framework.exts

import framework.db.*
import framework.sourcecode.DefinedAt

extension [A](connectionIO: ConnectionIO[A]) {

  /** Alias for `transact` that is less verbose. */
  def perform(using tx: Transactor[IO], tracer: Tracer[IO], definedAt: sourcecode.FullName): IO[A] =
    tracer.span(show"dbIO: ${definedAt.value}").surround(connectionIO.transact(tx))

  /** Raises an exception aborting the transaction if the value does not match. */
  def throwIfNot(value: A)(using definedAt: DefinedAt)(using CanEqual1[A]): ConnectionIO[Unit] =
    connectionIO.flatMap {
      case `value` => ConnectionIO.unit
      case other   => ConnectionIO.raiseError(new Exception(s"Expected $value, got $other at $definedAt"))
    }

  /** Raises an exception aborting the transaction if the value does not match. */
  def throwIfError(predicate: A => Option[String])(using definedAt: DefinedAt)(using CanEqual1[A]): ConnectionIO[Unit] =
    connectionIO.flatMap { value =>
      predicate(value) match {
        case None => ConnectionIO.unit
        case Some(error) =>
          ConnectionIO.raiseError(
            Exception(s"Expected predicate to pass for value $value at $definedAt, but received error: $error")
          )
      }
    }

  /** Returns true if the value matches. */
  def falseIfNot(value: A)(using CanEqual1[A]): ConnectionIO[Boolean] =
    connectionIO.map(_ == value)

  /** Returns an [[OptionT]] with `Some` if the value matches, `None` otherwise. */
  def valueOrNoneT(value: A)(using CanEqual1[A]): OptionT[ConnectionIO, A] =
    OptionT(connectionIO.map(actual => if (actual == value) Some(actual) else None))
}

extension (connectionIO: ConnectionIO[Int]) {

  /** Raises an exception aborting the transaction if the number of affected rows is not 1. */
  def singleOrThrow_!(using definedAt: DefinedAt): ConnectionIO[Unit] =
    connectionIO.throwIfNot(1)

  /** Returns true if the number of affected rows is 1. */
  def singleOrFalse: ConnectionIO[Boolean] =
    connectionIO.falseIfNot(1)

  /** Returns an [[OptionT]] with `Some` if the update was successful, `None` otherwise. */
  def singleOrNoneT: OptionT[ConnectionIO, Unit] =
    connectionIO.valueOrNoneT(1).void
}
