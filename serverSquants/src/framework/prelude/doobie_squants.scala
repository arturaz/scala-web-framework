package framework.prelude

import doobie.util.meta.Meta
import doobie.util.{Get, Put}
import framework.data.LengthIn
import squants.space.LengthUnit

given getLengthIn[U <: LengthUnit](using unit: ValueOf[U]): Get[LengthIn[U]] =
  Get[Double].map(LengthIn.fromValue)

given putLengthIn[U <: LengthUnit](using unit: ValueOf[U]): Put[LengthIn[U]] =
  Put[Double].contramap(_.valueIn(unit.value))

given metaLengthIn[U <: LengthUnit](using unit: ValueOf[U]): Meta[LengthIn[U]] =
  new Meta(getLengthIn, putLengthIn)
