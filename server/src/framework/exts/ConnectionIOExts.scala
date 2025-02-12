package framework.exts

import framework.db.*
import framework.sourcecode.DefinedAt

extension [A](connectionIO: ConnectionIO[A]) {

  /** Alias for `transact` that is less verbose. */
  def perform(using tx: Transactor[IO], tracer: Tracer[IO], definedAt: sourcecode.FullName): IO[A] =
    tracer.span(show"dbIO: ${definedAt.value}").surround(connectionIO.transact(tx))
}

extension (connectionIO: ConnectionIO[Int]) {

  /** Raises an exception aborting the transaction if the number of affected rows is not 1. */
  def singleOrThrow_!(using definedAt: DefinedAt): ConnectionIO[Unit] =
    connectionIO.flatMap {
      case 1     => ConnectionIO.unit
      case other => ConnectionIO.raiseError(new Exception(s"Expected 1 row, got $other at $definedAt"))
    }

  /** Returns true if the number of affected rows is 1. */
  def singleOrFalse: ConnectionIO[Boolean] =
    connectionIO.map(_ == 1)

  /** Returns an [[OptionT]] with `Some` if the update was successful, `None` otherwise. */
  def singleOrNoneT: OptionT[ConnectionIO, Unit] =
    OptionT(connectionIO.map(rows => if (rows == 1) Some(()) else None))
}
