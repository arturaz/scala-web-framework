package framework.prelude

import alleycats.Empty
import framework.exts.empty
import cats.data.{NonEmptyList, NonEmptyVector}

given [A: Empty]: Empty[NonEmptyVector[A]] = Empty(NonEmptyVector.one(empty[A]))
given [A: Empty]: Empty[NonEmptyList[A]] = Empty(NonEmptyList.one(empty[A]))
given emptyOption[A]: Empty[Option[A]] = Empty(None)
