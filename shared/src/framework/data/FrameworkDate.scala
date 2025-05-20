package framework.data

import cats.Show
import sttp.tapir.{Schema, SchemaType}

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, ZoneId}
import alleycats.Empty
import java.time.Month
import scala.util.Try
import java.time.LocalDateTime
import io.scalaland.chimney.Transformer
import framework.utils.FrameworkPlatform
import framework.exts.{*, given}
import framework.prelude.{*, given}
import cats.syntax.either.*
import framework.utils.UrlConvertible
import urldsl.errors.DummyError

/** A date in the local timezone. */
case class FrameworkDate(ld: LocalDate) extends AnyVal with Ordered[FrameworkDate] {
  override def compare(that: FrameworkDate): Int = ld.compareTo(that.ld)

  /** Returns the date in "yyyy-MM-dd" format. */
  def asString: String = ld.format(FrameworkDate.formatter)
}
object FrameworkDate {
  private val utc = ZoneId.of("UTC")
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  def nowUTC(): FrameworkDate = apply(LocalDate.now(utc))
  def nowUTCIO: SyncIO[FrameworkDate] = SyncIO(nowUTC())

  /** Returns the current date from the clients local timezone. */
  def nowClient(): FrameworkDate = apply(FrameworkPlatform.localDateNowClient())

  /** Returns the current date from the clients local timezone. */
  def nowClientIO: SyncIO[FrameworkDate] = SyncIO(nowClient())

  /** Parses a date in "yyyy-MM-dd" format. */
  def fromString(str: String): Either[String, FrameworkDate] = {
    Try(apply(formatter.parse(str, LocalDate.from))).toEither.leftMap(err =>
      s"Invalid date string \"${str.show}\": ${err.getMessage.show}"
    )
  }

  given Ordering[FrameworkDate] = Ordering.by(_.ld)

  given circeCodec: CirceCodec[FrameworkDate] = CirceCodec
    .fromUsing[Int]
    .imap(date => apply(LocalDate.of(date / 10000, Month.of(date / 100 % 100), date % 100)))(v => {
      val (year, month, day) = (v.ld.getYear(), v.ld.getMonthValue(), v.ld.getDayOfMonth())
      // 20240201
      year * 10000 + month * 100 + day
    })

  /** Parses a date in "yyyy-MM-dd" format. */
  def circeCodecIso: CirceCodec[FrameworkDate] =
    CirceCodec.fromUsing[String].iemap(date => fromString(date))(date => date.asString)

  given tapirCodec: TapirCodec[String, FrameworkDate, TapirCodecFormat.TextPlain] =
    TapirCodec.string.mapEither(str => Try(apply(LocalDate.parse(str, formatter))).toEither.leftMap(_.toString))(
      _.ld.format(formatter)
    )

  given schema: Schema[FrameworkDate] =
    Schema(SchemaType.SInteger()).description("A date in YYYYmmdd format").encodedExample("20241231")

  given Empty[FrameworkDate] = Empty(nowClient())

  trait Newtype extends yantl.Newtype.Of[FrameworkDate] {
    given Show[Type] = unwrap(_).asString

    given Ordering[Type] = Ordering.by(unwrap)

    given Transformer[Type, FrameworkDate] = unwrap
    given Transformer[FrameworkDate, Type] = make(_).getOrThrow

    given TapirCodec[String, Type, TapirCodecFormat.TextPlain] =
      FrameworkDate.tapirCodec.mapEither(make.asString)(unwrap)

    given UrlConvertible[Type, DummyError] = UrlConvertible.fromCodec

    given CanEqual1[Type] = CanEqual.derived
  }
  object Newtype {
    extension [A](obj: yantl.Newtype.WithType[FrameworkDate, A] & yantl.Newtype.WithoutValidation) {
      def nowUTC(): A = obj(FrameworkDate.nowUTC())
      def nowUTCIO: SyncIO[A] = SyncIO(obj.nowUTC())

      /** Returns the current date from the clients local timezone. */
      def nowClient(): A = obj(FrameworkDate.nowClient())

      /** Returns the current date from the clients local timezone. */
      def nowClientIO: SyncIO[A] = SyncIO(obj.nowClient())
    }
  }
}
