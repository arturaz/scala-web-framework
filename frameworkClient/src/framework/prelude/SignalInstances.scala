package framework.prelude

import cats.Applicative
import com.raquo.airstream.core.Signal

given Applicative[Signal] with
  override def pure[A](x: A): Signal[A] = Signal.fromValue(x)

  override def ap[A, B](ff: Signal[A => B])(fa: Signal[A]): Signal[B] =
    fa.combineWithFn(ff) { case (a, f) => f(a) }
