package framework.exts

import com.raquo.airstream.custom.CustomSource
import framework.sourcecode.DefinedAt

extension (stream: EventStream[Boolean]) {
  def collectTrues: EventStream[Unit] = stream.collect { case true => () }

  def collectFalses: EventStream[Unit] = stream.collect { case false => () }
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
}
