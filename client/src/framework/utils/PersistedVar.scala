package framework.utils

import com.raquo.airstream.core.{EventStream, Signal}
import com.raquo.airstream.state.{StrictSignal, Var}
import com.raquo.laminar.api.enrichSource
import com.raquo.laminar.inserters.DynamicInserter
import com.raquo.laminar.modifiers.Binder
import org.scalajs.dom.{window, Storage}

import scala.util.{Failure, Success, Try}

/** A [[Var]] that is persisted to the [[Storage]].
  *
  * @param underlying
  *   the underlying [[Var]] that holds the current value
  * @param persistenceKey
  *   the key that will be used to persist the value
  * @param storage
  *   the [[Storage]] that will be used to persist the value
  * @param defaultValueVar
  *   a [[Var]] that contains the default value
  */
class PersistedVar[A](
  val underlying: Var[A],
  persistenceKey: String,
  storage: Storage,
  defaultValueVar: Var[A],
)(using CanEqual1[A], CirceEncoder[A], CirceDecoder[A]) {
  def signal: StrictSignal[A] = underlying.signal

  /** Returns an [[EventStream]] that emits whenever the form is submitted or changed (with debounce). */
  def persistEvent(submitting: EventStream[Unit], underlyingDebounceMs: Int = 100): EventStream[A] =
    submitting
      .mapTo(underlying.now())
      .mergeWith(
        underlying.signal.changes.debounce(underlyingDebounceMs)
      )
      .distinct

  /** A binding that when bound to the DOM will start persisting the [[Var]]. */
  def persister(submitting: EventStream[Unit], underlyingDebounceMs: Int = 100): Binder.Base =
    persistEvent(submitting, underlyingDebounceMs) --> persist

  def setAndPersist(value: A): Unit = {
    underlying.set(value)
    persist(value)
  }

  def persist(value: A): Unit = {
    val serialized = value.asJson.noSpaces
    storage.setItem(persistenceKey, serialized)
  }

  /** A version of [[persistEvent]] that takes a [[Signal]]. */
  def persistEventFromSignal(submitting: Signal[Boolean], underlyingDebounceMs: Int = 100): EventStream[A] =
    persistEvent(submitting.changes.filter(identity).mapToUnit, underlyingDebounceMs)

  /** A version of [[persister]] that takes a [[Signal]]. */
  def persisterFromSignal(submitting: Signal[Boolean], underlyingDebounceMs: Int = 100): Binder.Base =
    persistEventFromSignal(submitting, underlyingDebounceMs) --> persist

  def isDifferentFromDefault: Boolean =
    underlying.signal.now() != defaultValueVar.now()

  lazy val differentFromDefaultSignal: Signal[Boolean] =
    underlying.signal.combineWithFn(defaultValueVar)(_ != _)

  lazy val maybeDifferentFromDefaultSignal: Signal[Option[Unit]] =
    underlying.signal.combineWithFn(defaultValueVar)((value, defaultValue) =>
      if (value != defaultValue) Some(()) else None
    )

  def resetToDefault(): Unit = {
    underlying.set(defaultValueVar.now())
    storage.removeItem(persistenceKey)
  }

  /** Changes the value that is considered to be the default value. */
  def changeDefaultTo(a: A): Unit = {
    defaultValueVar.set(a)
  }
}
object PersistedVar {
  given [A]: Conversion[PersistedVar[A], Var[A]] = _.underlying

  def apply[A](
    persistenceKey: String,
    defaultValue: => A,
    storage: Storage,
  )(using CanEqual[A, A], CirceEncoder[A], CirceDecoder[A]): PersistedVar[A] = {
    val maybeSerializedValue = Option(storage.getItem(persistenceKey))
    val currentValue = maybeSerializedValue match {
      case Some(serialized) =>
        decodeJsonAccumulating[A](serialized).fold(
          errors => {
            logError(
              s"""Failed to read persisted value for key "$persistenceKey" from session storage.
              |Using default value instead.
              |
              |${errors.iterator.map(_.show).mkString("\n")}
              |""".stripMargin
            )
            defaultValue
          },
          identity,
        )
      case None => defaultValue
    }
    new PersistedVar(Var(currentValue), persistenceKey, storage, Var(defaultValue))
  }

  /** Persists to the [[https://developer.mozilla.org/en-US/docs/Web/API/Window/sessionStorage session storage]]. */
  def session[A](
    persistenceKey: String,
    defaultValue: => A,
  )(using CirceDecoder[A], CirceEncoder[A], CanEqual[A, A]): PersistedVar[A] =
    apply(persistenceKey, defaultValue, window.sessionStorage)

  /** Persists to the [[https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage local storage]]. */
  def local[A](
    persistenceKey: String,
    defaultValue: => A,
  )(using CirceDecoder[A], CirceEncoder[A], CanEqual[A, A]): PersistedVar[A] =
    apply(persistenceKey, defaultValue, window.localStorage)
}
