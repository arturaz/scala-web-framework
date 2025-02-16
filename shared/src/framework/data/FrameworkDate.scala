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

  /** Returns the current date from the clients local timezone. */
  def nowClient(): FrameworkDate = apply(FrameworkPlatform.localDateNowClient())

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

    def nowUTC(): Type = make(FrameworkDate.nowUTC()).getOrThrow

    /** Returns the current date from the clients local timezone. */
    def nowClient(): Type = make(FrameworkDate.nowClient()).getOrThrow
  }
}
