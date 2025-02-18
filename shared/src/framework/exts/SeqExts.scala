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

  /** As [[Seq.find]] but returns an [[Option]] of the value and its index. */
  def findWithIndex(f: A => Boolean): Option[(A, Int)] =
    seq.iterator.zipWithIndex.find { case (a, _) => f(a) }
}
