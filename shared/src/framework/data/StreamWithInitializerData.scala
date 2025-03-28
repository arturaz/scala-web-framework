package framework.data

import sttp.tapir.Schema
import framework.prelude.*

/** Data for a stream that has a message that is sent to initialize the stream.
  *
  * For example, if you have a list of entries and then receive updates to those entries, you want to make sure that if
  * the stream is broken and then reestablished, the client will receive the initial list of entries again to make sure
  * we did not miss any events in between.
  */
enum StreamWithInitializerData[+InitData, +Event] derives CanEqual, Schema, CirceCodec {

  /** Sent when the stream is established. Will never be sent again after that. */
  case Initialize(data: InitData)

  /** The follow-up events. */
  case Event(event: Event)

  def mapInitData[NewInitData](f: InitData => NewInitData): StreamWithInitializerData[NewInitData, Event] =
    this match {
      case Initialize(data) => Initialize(f(data))
      case Event(event)     => Event(event)
    }

  def mapEvent[NewEvent](f: Event => NewEvent): StreamWithInitializerData[InitData, NewEvent] =
    this match {
      case Initialize(data) => Initialize(data)
      case Event(event)     => Event(f(event))
    }
}
