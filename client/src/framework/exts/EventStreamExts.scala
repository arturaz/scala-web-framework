package framework.exts

import com.raquo.airstream.custom.CustomSource
import framework.sourcecode.DefinedAt
import org.scalajs.dom.{ErrorEvent, EventSource, MessageEvent}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js.JSON

extension (stream: EventStream[Boolean]) {
  def collectTrues: EventStream[Unit] = stream.collect { case true => () }

  def collectFalses: EventStream[Unit] = stream.collect { case false => () }
}

extension [A](stream: EventStream[A]) {

  /** Combines `mapTo` and `toSignal`. */
  def mapToSignal[B](compute: => B): Signal[B] =
    stream.mapTo(compute).toSignal(compute)

  /** Combines `map` and `toSignal`. `compute` is called with [[None]] initially and then with the `Some(value)` from
    * the [[EventStream]].
    */
  def mapToSignal[B](compute: Option[A] => B): Signal[B] =
    stream.map(a => compute(Some(a))).toSignal(compute(None))
}

enum EventStreamInstruction[+A] {

  /** Emit a given value. */
  case Emit(value: A)

  /** Emit a sequence of values. */
  case EmitSeq(values: Seq[A])

  /** Wait for a given duration. */
  case Delay(time: FiniteDuration)
}
object EventStreamInstruction {

  /** Schedules a delay of 0ms. */
  def Yield: EventStreamInstruction[Nothing] = Delay(FiniteDuration(0, TimeUnit.MILLISECONDS))
}

extension (obj: EventStream.type) {

  /** As [[EventStream.fromCustomSource]] but stores the subscription for you. */
  def fromCustomSourceWithSubscription[A, TSubscription](
    shouldStart: CustomSource.StartIndex => Boolean = _ => true,
    start: (
      CustomSource.FireValue[A],
      CustomSource.FireError,
      CustomSource.GetStartIndex,
      CustomSource.GetIsStarted,
    ) => TSubscription,
    stop: (CustomSource.StartIndex, TSubscription) => Unit,
  )(using DefinedAt): EventStream[A] = {
    var maybeSubscription = Option.empty[TSubscription]
    EventStream.fromCustomSource(
      shouldStart,
      start = (fireValue, fireError, getStartIndex, getIsStarted) => {
        val subscription = start(fireValue, fireError, getStartIndex, getIsStarted)
        maybeSubscription = Some(subscription)
      },
      stop = startIndex => {
        maybeSubscription match {
          case None =>
            logWarning("No subscription to stop")
          case Some(subscription) =>
            stop(startIndex, subscription)
            maybeSubscription = None
        }
      },
    )
  }

  /** Creates an [[EventStream]] from a DOM event source. */
  def fromDomEventSource(
    create: => EventSource,
    convertError: ErrorEvent => Throwable = { evt =>
      new Exception(s"An error occurred while getting events from EventSource: ${JSON.stringify(evt, space = 2)}")
    },
  ): EventStream[MessageEvent] = {
    fromCustomSourceWithSubscription(
      start = (fireValue: CustomSource.FireValue[MessageEvent], fireError, getStartIndex, getIsStarted) => {
        val evtSource = create
        evtSource.onmessage = evt => fireValue(evt)
        evtSource.onerror = evt => fireError(convertError(evt))
        evtSource
      },
      stop = (_, evtSource) => {
        evtSource.close()
      },
    )
  }

  /** Creates an [[EventStream]] from a DOM event source which emits `Right([[MessageEvent]])` and then finally
    * `Left([[ErrorEvent]])`.
    */
  def fromDomEventSourceEither(
    create: => EventSource
  ): EventStream[Either[ErrorEvent, MessageEvent]] = {
    fromCustomSourceWithSubscription(
      start =
        (fireValue: CustomSource.FireValue[Either[ErrorEvent, MessageEvent]], fireError, getStartIndex, getIsStarted) =>
          {
            val evtSource = create
            evtSource.onmessage = evt => fireValue(Right(evt))
            evtSource.onerror = evt => fireValue(Left(evt))
            evtSource
          },
      stop = (_, evtSource) => {
        evtSource.close()
      },
    )
  }

  /** Create an [[EventStream]] from a sequence of [[EventStreamEvent]].
    *
    * Example:
    * {{{
    * EventStream.fromInstructions(Delay(5.seconds), Emit("foo"), Delay(10.seconds), Emit("bar"))
    * }}}
    *
    * @param emitOnce
    *   see [[EventStream.fromValue]]
    */
  def fromInstructions[A](
    emitOnce: Boolean = false,
    instructions: EventStreamInstruction[A]*
  ): EventStream[A] =
    fromInstructionsSeq(_ => instructions, emitOnce)

  /** @see [[fromInstructions]] */
  def fromInstructionsSeq[A](
    createInstructions: EventStreamInstruction.type => Seq[EventStreamInstruction[A]],
    emitOnce: Boolean = false,
  ): EventStream[A] = {
    val instructions = createInstructions(EventStreamInstruction)
    var currentDelay = FiniteDuration(0, TimeUnit.MILLISECONDS)

    val streams = instructions.flatMap {
      case EventStreamInstruction.Delay(time) =>
        currentDelay += time
        None

      case EventStreamInstruction.Emit(value) =>
        val stream =
          if (currentDelay.length == 0) EventStream.fromValue(value, emitOnce)
          else EventStream.delay(currentDelay.toMillis.toInt, value, emitOnce)

        Some(stream)

      case EventStreamInstruction.EmitSeq(values) =>
        val stream =
          if (currentDelay.length == 0) EventStream.fromSeq(values, emitOnce)
          else
            EventStream
              .delay(currentDelay.toMillis.toInt, (), emitOnce)
              .flatMapSwitch(_ => EventStream.fromSeq(values, emitOnce))

        Some(stream)
    }

    EventStream.mergeSeq(streams)
  }
}
