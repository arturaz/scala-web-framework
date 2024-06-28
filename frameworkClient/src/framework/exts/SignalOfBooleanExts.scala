package framework.exts

import alleycats.Empty
import com.raquo.airstream.extensions.BooleanSignal

extension (signal: Signal[Boolean]) {

  /** Splits the [[Signal]] by the [[Boolean]] cases, returning an [[Option]] for use with `child.maybe`.
    *
    * The [[Signal]] emits every time the [[Boolean]] becomes true.
    */
  def splitBooleanAsOption[B](
    whenTrue: Signal[Unit] => B
  ): Signal[Option[B]] = {
    signal.map(if (_) Some(()) else None).splitOption((_, signal) => whenTrue(signal))
  }

  /** An version of [[BooleanSignal.splitBoolean]] that uses [[Empty]] to produce a value when the [[Boolean]] is false.
    */
  def splitBooleanOrEmpty[B](
    whenTrue: Signal[Unit] => B
  )(using empty: Empty[B]): Signal[B] =
    signal.splitBoolean(whenTrue, whenFalse = _ => empty.empty)
}
