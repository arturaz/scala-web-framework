package framework.prelude

import scala.annotation.nowarn

export cats.{~>, Id, Show}
export cats.data.{EitherT, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector, OptionT}
export cats.effect.{IO, Resource, SyncIO}
export sttp.tapir.Schema

export cats.syntax.show.{showInterpolator, toShow}
export cats.syntax.invariant.*
export cats.syntax.apply.*
export cats.syntax.reducible.*
export cats.syntax.functor.*
export cats.syntax.flatMap.*
export cats.syntax.comonad.*
export cats.syntax.monoid.*

export alleycats.syntax.all.*

export neotype.Newtype

/** A type alias that allows writing code like this:
  *
  * {{{
  *   def myFn[A: CanEqual1]: Int = ...
  * }}}
  *
  * @see
  *   https://docs.scala-lang.org/scala3/reference/contextual/multiversal-equality.html#why-two-type-parameters-1
  */
type CanEqual1[A] = CanEqual[A, A]
