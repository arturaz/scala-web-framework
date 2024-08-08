package framework.exts

import alleycats.Empty
import framework.prelude.CanEqual1

/** Allows using `empty[Type]` instead of `Empty[Type].empty`. */
def empty[A](using empty: Empty[A]): A = empty.empty

/** Alias for [[empty]]. */
def emptyValue[A](using empty: Empty[A]): A = empty.empty

extension [A](a: A) {

  /** Returns whether the value is the same as the [[Empty]] value. */
  def isEmpty(using e: Empty[A], eq: CanEqual1[A]): Boolean = a == e.empty

  /** Returns whether the value is not the same as the [[Empty]] value. */
  def isNonEmpty(using e: Empty[A], eq: CanEqual1[A]): Boolean = a != e.empty

  /** Returns [[Some]] if the value is not the same as the [[Empty]] value. */
  def nonEmptyOption(using e: Empty[A], eq: CanEqual1[A]): Option[A] = if (a == e.empty) None else Some(a)
}
