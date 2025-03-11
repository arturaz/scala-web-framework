package framework.prelude

import hedgehog.{Gen, Range}
import java.util.UUID

export jkugiya.ulid.genULID

given genUUID: Gen[UUID] = for {
  mostSignificantBits <- Gen.long(Range.linear(Long.MinValue, Long.MaxValue))
  leastSignificantBits <- Gen.long(Range.linear(Long.MinValue, Long.MaxValue))
} yield UUID(mostSignificantBits, leastSignificantBits)

given newTypeGen[TUnderlying, TWrapper](using
  newType: yantl.Newtype.WithUnvalidatedType[TUnderlying, TWrapper],
  gen: Gen[TUnderlying],
): Gen[TWrapper] =
  gen.map(newType.apply)
