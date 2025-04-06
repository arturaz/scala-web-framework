package framework.exts

import framework.data.MaybeSignal

extension [A](value: MaybeSignal[A]) {

  /** Deunionsizes the value into a [[Signal]] if it is not already one. */
  inline def deunionizeSignal: Signal[A] =
    value match {
      case s: Signal[A @unchecked] => s
      case other                   => Signal.fromValue(other.asInstanceOf)
    }

  inline def fold[B](ifSignal: Signal[A] => B, ifValue: A => B): B =
    value match {
      case s: Signal[A @unchecked] => ifSignal(s)
      case other                   => ifValue(other.asInstanceOf)
    }
}
