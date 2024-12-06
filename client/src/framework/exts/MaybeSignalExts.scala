package framework.exts

import framework.data.MaybeSignal

extension [A](value: MaybeSignal[A]) {

  /** Deunionsizes the value into a [[Signal]] if it is not already one. */
  def deunionizeSignal: Signal[A] =
    value match {
      case s: Signal[A @unchecked] => s
      case other                   => Signal.fromValue(other.asInstanceOf[A])
    }
}
