package framework.data

import cats.Show
import sttp.tapir.{Schema, SchemaType}

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId, ZonedDateTime}
import scala.util.Try
import java.time.temporal.ChronoUnit
import framework.exts.{*, given}
import framework.prelude.{*, given}
import cats.syntax.either.*
import framework.utils.FrameworkPlatform
import scala.concurrent.duration.*
import framework.utils.UrlConvertible
import urldsl.errors.DummyError

/** A timestamp in the UTC timezone with the precision of milliseconds.
  *
  * This precision is used to make sure we do not have problems with databases which often will only store milliseconds
  * precision.
  */
case class FrameworkDateTime private (ldt: LocalDateTime) extends AnyVal with Ordered[FrameworkDateTime] {
  override def compare(that: FrameworkDateTime): Int = ldt.compareTo(that.ldt)

  def toDate: FrameworkDate = FrameworkDate(ldt.toLocalDate)

  /** Returns true if the given [[FrameworkDateTime]] is on the same day. */
  def isSameDay(at: FrameworkDateTime): Boolean = ldt.toLocalDate == at.ldt.toLocalDate

  /** Returns true if the given [[FrameworkDate]] is on the same day. */
  def isSameDay(at: FrameworkDate): Boolean = ldt.toLocalDate == at.ld

  def toZonedDateTime: ZonedDateTime = ldt.atZone(FrameworkDateTime.utc)
  def toInstant: Instant = toZonedDateTime.toInstant

  /** Returns the value as you would get it from [[cats.effect.Clock.realTime]]. */
  def toFiniteDuration: FiniteDuration = FiniteDuration(toInstant.toEpochMilli(), MILLISECONDS)

  /** Returns the timestamp in "yyyy-MM-dd HH:mm:ss" format. */
  def asString: String = s"${ldt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")).show} UTC"

  /** Returns the timestamp from the unix epoch in milliseconds. */
  def toUnixMillis: Long = toInstant.toEpochMilli

  def +(duration: FiniteDuration): FrameworkDateTime =
    FrameworkDateTime(ldt.plusNanos(duration.toNanos))

  def -(duration: FiniteDuration): FrameworkDateTime =
    FrameworkDateTime(ldt.minusNanos(duration.toNanos))

  def -(other: FrameworkDateTime): FiniteDuration =
    ChronoUnit.MILLIS.between(other.toZonedDateTime, toZonedDateTime).millis
}
object FrameworkDateTime {
  private val utc = ZoneId.of("UTC")
  private val fromToStringFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd_HH:mm:ss.SSS")

  def apply(ldt: LocalDateTime): FrameworkDateTime =
    new FrameworkDateTime(ldt.truncatedTo(ChronoUnit.MILLIS))

  def fromInstant(instant: Instant): FrameworkDateTime =
    apply(LocalDateTime.ofInstant(instant, utc))

  def fromUnixMillis(millis: Long): FrameworkDateTime =
    fromInstant(Instant.ofEpochMilli(millis))

  def now(): FrameworkDateTime = apply(LocalDateTime.now(utc))

  /** Returns the current date from the clients local timezone. */
  def nowClient(): FrameworkDateTime = apply(FrameworkPlatform.localDateTimeNowClient())

  val nowIO: SyncIO[FrameworkDateTime] = SyncIO(now())

  given Ordering[FrameworkDateTime] = Ordering.by(_.ldt)

  given CanEqual1[FrameworkDateTime] = CanEqual.derived

  given circeCodec: CirceCodec[FrameworkDateTime] =
    CirceCodec
      .fromUsing[Long]
      .imap(millis => apply(LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), utc)))(_.toInstant.toEpochMilli)

  given tapirCodec: TapirCodec[String, FrameworkDateTime, TapirCodecFormat.TextPlain] =
    TapirCodec.string.mapEither(str =>
      Try(apply(LocalDateTime.parse(str, fromToStringFormatter))).toEither.leftMap(_.toString)
    )(_.ldt.format(fromToStringFormatter))

  given schema: Schema[FrameworkDateTime] =
    Schema(SchemaType.SInteger())
      .description("Milliseconds passed since the start of Unix epoch")
      .encodedExample("1719314788000")

  given show: Show[FrameworkDateTime] = _.asString

  trait Newtype extends yantl.Newtype.Of[FrameworkDateTime] {
    given Show[Type] = unwrap(_).asString

    given Ordering[Type] = Ordering.by(unwrap)

    given Conversion[Type, FrameworkDateTime] = unwrap

    given CanEqual1[Type] = CanEqual.derived

    given TapirCodec[String, Type, TapirCodecFormat.TextPlain] =
      FrameworkDateTime.tapirCodec.mapEither(makeAsString)(unwrap)

    given UrlConvertible[Type, DummyError] = UrlConvertible.fromCodec

    def schemaFor: Schema[Type] = FrameworkDateTime.schema.map(v => make(v).toOption)(unwrap)
  }
  object Newtype {
    extension [A](obj: yantl.Newtype.WithType[FrameworkDateTime, A] & yantl.Newtype.WithoutValidation) {
      def now(): A = obj.apply(FrameworkDateTime.now())
      def nowIO: SyncIO[A] = SyncIO(now())
    }
  }

  given asInstant: Conversion[FrameworkDateTime, Instant] = _.toInstant
}
