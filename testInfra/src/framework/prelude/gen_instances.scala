package framework.prelude

import framework.exts.*
import framework.data.{FrameworkDate, FrameworkDateTime, LatLng, Latitude, Longitude}
import hedgehog.{Gen, Range}

import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}
import java.util.UUID
import sttp.model.Uri

export jkugiya.ulid.genULID

given newTypeGen[TUnderlying, TWrapper](using
  newType: yantl.Newtype.WithUnvalidatedType[TUnderlying, TWrapper],
  gen: Gen[TUnderlying],
): Gen[TWrapper] =
  gen.map(newType.apply)

given genUUID: Gen[UUID] = for {
  mostSignificantBits <- Gen.long(Range.linear(Long.MinValue, Long.MaxValue))
  leastSignificantBits <- Gen.long(Range.linear(Long.MinValue, Long.MaxValue))
} yield UUID(mostSignificantBits, leastSignificantBits)

given genUri: Gen[Uri] = for {
  scheme <- Gen.choice1(Gen.constant("http"), Gen.constant("https"))
  hostname <- Gen.string(Gen.alpha, Range.linear(1, 20))
  tld <- Gen.choice1(Gen.constant("com"), Gen.constant("net"), Gen.constant("org"))
  host = show"$hostname.$tld"
} yield Uri.safeApply(scheme, host).getOrThrow

given genLocalDate: Gen[LocalDate] =
  Gen
    .long(Range.linear(Instant.MIN.getEpochSecond(), Instant.MAX.getEpochSecond()))
    .map(Instant.ofEpochSecond)
    .map(LocalDate.ofInstant(_, ZoneId.of("Z")))

given genLocalDateTime: Gen[LocalDateTime] =
  Gen
    .long(Range.linear(Instant.MIN.getEpochSecond(), Instant.MAX.getEpochSecond()))
    .map(Instant.ofEpochSecond)
    .map(LocalDateTime.ofInstant(_, ZoneId.of("Z")))

given genFrameworkDate: Gen[FrameworkDate] =
  summon[Gen[LocalDate]].map(FrameworkDate.apply)

given genFrameworkDateTime: Gen[FrameworkDateTime] =
  summon[Gen[LocalDateTime]].map(FrameworkDateTime.apply)

given genLatitude: Gen[Latitude] =
  Gen.double(Range.linearFrac(Latitude.MinValue, Latitude.MaxValue)).map(Latitude.make.orThrow)

given genLongitude: Gen[Longitude] =
  Gen.double(Range.linearFrac(Longitude.MinValue, Longitude.MaxValue)).map(Longitude.make.orThrow)

given genLatLng: Gen[LatLng] =
  for {
    lat <- genLatitude
    lng <- genLongitude
  } yield LatLng(lat, lng)
