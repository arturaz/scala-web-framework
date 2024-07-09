package framework.exts

import framework.db.*
import framework.sourcecode.DefinedAt

extension [A](connectionIO: ConnectionIO[A]) {

  /** Alias for `transact` that is less verbose. */
  def perform(using tx: Transactor[IO]): IO[A] = connectionIO.transact(tx)
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
}
