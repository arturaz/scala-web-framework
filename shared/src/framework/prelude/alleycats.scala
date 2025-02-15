package framework.prelude

import alleycats.Empty
import framework.exts.empty
import cats.data.{NonEmptyList, NonEmptyVector}

given nonEmptyVectorOfEmptyValue[A: Empty]: Empty[NonEmptyVector[A]] = Empty(NonEmptyVector.one(empty[A]))
given nonEmptyListOfEmptyValue[A: Empty]: Empty[NonEmptyList[A]] = Empty(NonEmptyList.one(empty[A]))
given emptyOption[A]: Empty[Option[A]] = Empty(None)
