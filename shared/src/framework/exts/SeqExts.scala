package framework.exts

extension [A](seq: Seq[A]) {

  /** Upcasts the type to [[Seq]] from a specific type. */
  def asSeq: Seq[A] = seq
}
