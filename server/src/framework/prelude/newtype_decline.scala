package framework.prelude

import com.monovore.decline.Argument
import cats.data.Validated

given argumentForNewtype[TUnderlying, TWrapper](using
  newType: yantl.Newtype.WithType[TUnderlying, TWrapper],
  arg: Argument[TUnderlying],
): Argument[TWrapper] =
  arg.emap(underlying =>
    Validated.fromEither(
      newType.make.asStrings(underlying).left.map(NonEmptyList.fromFoldable(_).getOrThrow("impossible"))
    )
  )
