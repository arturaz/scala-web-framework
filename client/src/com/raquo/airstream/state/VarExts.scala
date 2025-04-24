package com.raquo.airstream.state

import com.raquo.airstream.core.AirstreamError.VarError
import com.raquo.airstream.core.{AirstreamError, Transaction}

import scala.util.{Failure, Success, Try}

trait FrameworkVarExts {
  extension [A](rxVar: Var[A]) {

    /** Updates the value if the given function returns `Some`.
      *
      * @note
      *   Do not use on failed Vars. Use [[tryUpdate]] on those.
      * @note
      *   Note: guarded against exceptions
      */
    def maybeUpdate(mod: A => Option[A]): Unit = {
      Transaction { trx =>
        rxVar.tryNow() match {
          case Success(currentValue) =>
            val nextValue = Try(mod(currentValue)) // this does catch exceptions in mod(currentValue)
            nextValue match {
              case Success(None)            => // do nothing
              case Success(Some(nextValue)) => rxVar.setCurrentValue(Success(nextValue), trx)
              case Failure(err)             => rxVar.setCurrentValue(Failure(err), trx)
            }

          case Failure(err) =>
            AirstreamError.sendUnhandledError(
              VarError("Unable to update a failed Var. Consider Var#tryUpdate instead.", cause = Some(err))
            )
        }
      }
    }

    /** Updates the value if the given function returns `Some`.
      *
      * @param mod
      *   Note: must not throw
      */
    def tryMaybeUpdate(mod: Try[A] => Option[Try[A]]): Unit = {
      Transaction { trx =>
        mod(rxVar.getCurrentValue) match {
          case None        => // do nothing
          case Some(value) => rxVar.setCurrentValue(value, trx)
        }
      }
    }
  }
}

given frameworkVarExts: FrameworkVarExts = new FrameworkVarExts {}
