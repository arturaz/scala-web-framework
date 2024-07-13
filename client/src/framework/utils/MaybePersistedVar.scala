package framework.utils

import com.raquo.airstream.state.StrictSignal

/** A [[Var]] that is potentially persisted. */
opaque type MaybePersistedVar[A] = PersistedVar[A] | Var[A]
object MaybePersistedVar {
  given [A]: Conversion[PersistedVar[A], MaybePersistedVar[A]] = apply
  given [A]: Conversion[Var[A], MaybePersistedVar[A]] = apply

  def apply[A](persisted: PersistedVar[A]): MaybePersistedVar[A] = persisted
  def apply[A](rxVar: Var[A]): MaybePersistedVar[A] = rxVar

  extension [A](maybe: MaybePersistedVar[A]) {
    def asPersisted: Option[PersistedVar[A]] = maybe match
      case persisted: PersistedVar[A @unchecked] => Some(persisted)
      case _: Var[A @unchecked]                  => None

    def rxVar: Var[A] = maybe match
      case persisted: PersistedVar[A @unchecked] => persisted.underlying
      case rxVar: Var[A @unchecked]              => rxVar

    def signal: StrictSignal[A] =
      rxVar.signal
  }
}
