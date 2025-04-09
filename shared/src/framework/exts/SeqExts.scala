package framework.exts

extension [A](seq: Seq[A]) {

  /** Upcasts the type to [[Seq]] from a specific type. */
  def asSeq: Seq[A] = seq

  /** As [[Seq.indexOf]] but returns an [[Option]]. */
  def indexOfOption(elem: A, from: Int = 0): Option[Int] =
    seq.indexOf(elem, from) match {
      case -1  => None
      case idx => Some(idx)
    }

  /** As [[Seq.indexWhere]] but returns an [[Option]]. */
  def indexWhereOption(f: A => Boolean, from: Int = 0): Option[Int] =
    seq.indexWhere(f, from) match {
      case -1  => None
      case idx => Some(idx)
    }
}
