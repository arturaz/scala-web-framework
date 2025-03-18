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

  /** As [[Seq.find]] but returns an [[Option]] of the value and its index. */
  def findWithIndex(f: A => Boolean): Option[(A, Int)] =
    seq.iterator.zipWithIndex.find { case (a, _) => f(a) }

  /** Finds the first element that returns [[Some]] from the given function. */
  def findMap[B](f: A => Option[B]): Option[B] =
    seq.iterator.map(f).find(_.isDefined).flatten
}
