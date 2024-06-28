package framework.exts

import alleycats.Empty

/** Allows using `empty[Type]` instead of `Empty[Type].empty`. */
def empty[A](using empty: Empty[A]): A = empty.empty

extension [A](a: A) {

  /** Returns whether the value is the same as the [[Empty]] value. */
  def isEmpty(using e: Empty[A], eq: CanEqual[A, A]): Boolean = a == e.empty

  /** Returns whether the value is not the same as the [[Empty]] value. */
  def isNonEmpty(using e: Empty[A], eq: CanEqual[A, A]): Boolean = a != e.empty
}
