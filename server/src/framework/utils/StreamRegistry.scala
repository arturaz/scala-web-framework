package framework.utils

import cats.syntax.all.*
import cats.effect.Concurrent
import cats.effect.kernel.{MonadCancelThrow, Ref}
import org.typelevel.otel4s.metrics.{MeasurementValue, Meter, MeterProvider, UpDownCounter}
import framework.data.AttributeWithCirceEncoder

import StreamRegistry.{StreamCount, StreamDetails, StreamName, StreamStats}
import cats.Applicative
import sttp.tapir.EndpointMetaOps
import io.circe.Json
import org.typelevel.otel4s.Attribute
import scala.annotation.targetName
import cats.Traverse
import alleycats.Empty
import cats.effect.kernel.Unique
import monocle.syntax.all.*

/** Tracks ongoing streams. */
trait StreamRegistry[F[_]] {

  /** Registers a stream under a name.
    *
    * @param name
    *   the name of the stream.
    * @param details
    *   the optional details of the stream that will be visible in [[get]] and as metrics attributes.
    * @param s
    *   the stream.
    */
  def register[A](name: StreamName, details: Option[StreamDetails], s: Stream[F, A]): Stream[F, A]

  /** Gets the current streams. */
  def get: F[Map[StreamName, StreamStats]]
}
object StreamRegistry {
  final val StreamNamePrefix = "app.streams"

  private def makeRef[F[_]: Concurrent] =
    Ref.of[F, Map[StreamName, StreamStats]](Map.empty)

  /** Creates a stream registry. */
  def apply[F[_]: Concurrent: MeterProvider]: F[StreamRegistry[F]] = for {
    meter <- MeterProvider[F]().meter("framework.utils.StreamRegistry").get
    totalCounter <- meter
      .upDownCounter[StreamCount](show"$StreamNamePrefix.total")
      .withDescription("Total number of streams.")
      .withUnit("stream")
      .create
    ref <- makeRef[F]
  } yield new StreamRegistryImpl(ref, Some(meter), Some(totalCounter))

  /** Creates a stream registry without metrics. */
  def withoutMetrics[F[_]: Concurrent]: F[StreamRegistry[F]] = for {
    ref <- makeRef[F]
  } yield new StreamRegistryImpl(ref, meter = None, totalCounter = None)

  /** A stream registry that does nothing. */
  def noOp[F[_]: Applicative]: StreamRegistry[F] = new {
    override def register[A](name: StreamName, details: Option[StreamDetails], s: Stream[F, A]): Stream[F, A] = s

    override def get: F[Map[StreamName, StreamStats]] = Map.empty.pure
  }

  /** A name for the stream which is used in metrics. Should not include details that vary with each request. */
  object StreamName extends NewtypeNonEmptyString {
    def fromEndpoint(e: EndpointMetaOps): StreamName =
      makeOrThrow(e.showShort)

    def fromEndpoint(prefix: String, e: EndpointMetaOps): StreamName =
      makeOrThrow(show"$prefix${e.showShort}")
  }
  type StreamName = StreamName.Type

  /** The number of streams with a given name. */
  object StreamCount extends NewtypeLong with Newtype.WithoutValidation {
    extension (n: Type) {
      def increment: StreamCount = StreamCount(n + 1)
      def decrement: StreamCount = StreamCount(n - 1)
    }

    given MeasurementValue[StreamCount] = MeasurementValue.longMeasurementValue.contramap(_.unwrap)
  }
  type StreamCount = StreamCount.Type

  /** The number of events in a stream. */
  object StreamEventCount extends NewtypeLong with Newtype.WithoutValidation {
    extension (v: Type) {
      def increment(n: Long): StreamEventCount = StreamEventCount(v + n)
    }
  }
  type StreamEventCount = StreamEventCount.Type

  /** The details of a stream. */
  case class StreamDetails(events: StreamEventCount, attributes: Seq[AttributeWithCirceEncoder[?]])
  object StreamDetails {
    @targetName("applySeq")
    def apply(attributes: AttributeWithCirceEncoder[?]*): StreamDetails = apply(StreamEventCount(0), attributes.toSeq)

    val empty: StreamDetails = apply(StreamEventCount(0), Nil)
    given Empty[StreamDetails] = Empty(empty)

    given CirceEncoder[StreamDetails] = v =>
      Json.obj(
        (Iterator("events" -> Json.fromLong(v.events.unwrap)) ++ v.attributes.iterator.map(_.toJsonKV)).toArray*
      )
  }

  /** Instrument names must consist of 255 or fewer characters including alphanumeric, _, ., -, /, and start with a
    * letter.
    */
  def sanitizeStreamName(name: String): String =
    name.headOption match {
      case None                   => "unnamed_stream"
      case Some(c) if !c.isLetter => sanitizeStreamName(s"s$name")
      case Some(_)                => name.replaceAll("[^a-zA-Z0-9\\._\\-/]", "_").take(255)
    }

  case class StreamStats(count: StreamCount, details: Map[Unique.Token, StreamDetails]) {
    def addStream(unique: Unique.Token, details: Option[StreamDetails]): StreamStats =
      copy(count = count.increment, details = this.details.updated(unique, details.getOrEmpty))

    def removeStream(unique: Unique.Token): StreamStats =
      copy(count = count.decrement, details = this.details - unique)

    def eventsFor(unique: Unique.Token, n: Long): StreamStats =
      this.focus(_.details.index(unique).events).modify(_.increment(n))
  }
  object StreamStats {
    def empty: StreamStats = apply(StreamCount(0), Map.empty)

    given Empty[StreamStats] = Empty(empty)

    given CirceEncoder[StreamStats] = v =>
      Json.obj(
        "count" -> Json.fromLong(v.count.unwrap),
        "streams" -> v.details.valuesIterator.map(_.asJson).toVector.asJson,
      )
  }
}

class StreamRegistryImpl[F[_]: MonadCancelThrow: Unique](
  ref: Ref[F, Map[StreamName, StreamStats]],
  meter: Option[Meter[F]],
  totalCounter: Option[UpDownCounter[F, StreamCount]],
) extends StreamRegistry[F] {
  override def register[A](name: StreamName, details: Option[StreamDetails], s: Stream[F, A]): Stream[F, A] = {
    def createCounter = meter.map(
      _.upDownCounter[StreamCount](
        StreamRegistry.sanitizeStreamName(show"${StreamRegistry.StreamNamePrefix}.stream.$name")
      )
        .withDescription("Number of streams with the given name.")
        .withUnit("stream")
        .create
    )

    val attributes = details.iterator.flatMap(_.attributes.iterator).map(_.attribute).toVector

    val bracketStream = Stream
      .bracket(
        for {
          unique <- Unique[F].unique
          _ <- ref.update { map =>
            map.updatedWith(name) {
              case None        => Some(StreamStats(StreamCount(1), details.map(unique -> _).toMap))
              case Some(stats) => Some(stats.addStream(unique, details))
            }
          }
          _ <- totalCounter.map(_.inc(attributes*)).sequence_
          maybeCounter <- createCounter.map(_.flatTap(_.inc(attributes*))).sequence
        } yield (maybeCounter, unique)
      ) { case (maybeCounter, unique) =>
        for {
          _ <- ref.update { map =>
            map.updatedWith(name) {
              case None                                 => None // should never happen
              case Some(StreamStats(StreamCount(1), _)) => None
              case Some(stats)                          => Some(stats.removeStream(unique))
            }
          }
          _ <- totalCounter.map(_.dec(attributes*)).sequence_
          _ <- maybeCounter.fold2(Applicative[F].unit, _.dec(attributes*))
        } yield ()
      }
      .map(_._2)

    for {
      unique <- bracketStream
      event <- s.evalTapChunks(chunk =>
        ref.update { map =>
          map.updatedWith(name) {
            case None        => None // should never happen
            case Some(stats) => Some(stats.eventsFor(unique, chunk.size))
          }
        }
      )
    } yield event
  }

  override def get: F[Map[StreamName, StreamStats]] = ref.get
}
