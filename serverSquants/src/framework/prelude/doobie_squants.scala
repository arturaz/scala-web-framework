package framework.prelude

import doobie.util.{Get, Put}
import framework.data.LengthIn
import squants.space.LengthUnit

given [U <: LengthUnit](using unit: ValueOf[U]): Get[LengthIn[U]] =
  Get[Double].map(LengthIn.fromValue)

given [U <: LengthUnit](using unit: ValueOf[U]): Put[LengthIn[U]] =
  Put[Double].contramap(_.valueIn(unit.value))
