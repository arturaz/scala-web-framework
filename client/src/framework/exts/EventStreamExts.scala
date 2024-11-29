package framework.exts

import com.raquo.airstream.custom.CustomSource
import framework.sourcecode.DefinedAt
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit

extension (stream: EventStream[Boolean]) {
  def collectTrues: EventStream[Unit] = stream.collect { case true => () }

  def collectFalses: EventStream[Unit] = stream.collect { case false => () }
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
