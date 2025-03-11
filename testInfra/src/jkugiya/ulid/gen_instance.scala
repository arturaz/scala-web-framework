package jkugiya.ulid

import hedgehog.{Gen, Range}

// Defined here because the constants we access are package private.

given genULID: Gen[ULID] = for {
  randomness <- Gen.bytes(Range.singleton(ULID.RandomnessSize))
  time <- Gen.long(Range.linear(ULID.MinTimestamp, ULID.MaxTimestamp))
} yield ULID(time, randomness)
