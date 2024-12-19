package framework.exts

extension [A](io: SyncIO[A]) {

  /** Evaluates the [[SyncIO]] and returns the value as a [[Signal]]. */
  def toSignal(): Signal[A] = Signal.fromValue(io.unsafeRunSync())
}
