package framework.prelude

import hedgehog.{Gen, Range}
import squants.space.{Length, Meters}

given genLength: Gen[Length] =
  Gen.double(Range.linearFrac(0.0, 10000)).map(Meters(_))
