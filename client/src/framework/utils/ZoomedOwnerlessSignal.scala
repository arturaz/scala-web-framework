package framework.utils

import cats.Invariant

/** The signal and a way to set it bundled together. Weaker version of [[UpdatableSignal]]. */
trait ZoomedOwnerlessSignal[A] {

  /** The [[Signal]] which is most likely mapped from a [[Var]]. */
  def signal: Signal[A]

  /** Sets the value of the underlying data source to the given value. */
  def setTo(value: A): Unit
}
object ZoomedOwnerlessSignal {

  /** Creates an instance. */
  def apply[A](
    signal: Signal[A],
    setTo: A => Unit,
  ): ZoomedOwnerlessSignal[A] = {
    val s = signal
    val set = setTo

    new ZoomedOwnerlessSignal[A] {
      override def signal: Signal[A] = s
      override def setTo(value: A): Unit = set(value)
    }
  }

  /** Creates an instance from a [[Var]] and mapping functions. */
  given fromVar[A]: Conversion[Var[A], ZoomedOwnerlessSignal[A]] = rxVar => apply[A](rxVar.signal, rxVar.set)

  given Invariant[ZoomedOwnerlessSignal] with {
    override def imap[A, B](self: ZoomedOwnerlessSignal[A])(f: A => B)(g: B => A): ZoomedOwnerlessSignal[B] =
      new ZoomedOwnerlessSignal[B] {
        override val signal: Signal[B] = self.signal.map(f)
        override def setTo(value: B): Unit = self.setTo(g(value))
      }
  }
}
