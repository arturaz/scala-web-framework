package framework.utils

import com.raquo.airstream.core.Signal
import com.raquo.airstream.split.SplittableSignal
import com.raquo.airstream.state.Var

/** Like [[Var.zoom]] but is not required to have an [[com.raquo.airstream.ownership.Owner]]. */
trait UpdatableSignal[A] { self =>

  /** The [[Signal]] which is most likely mapped from a [[Var]]. */
  def signal: Signal[A]

  /** The current value. */
  def now(): A

  /** Sets the value of the underlying data source to the given value. */
  def setTo(value: A): Unit

  /** Updates the value of the underlying data source using the given function. */
  def update(f: A => A): Unit = {
    val a = now()
    val newA = f(a)
    setTo(newA)
  }

  /** Returns a new [[UpdatableSignal]] which is mapped.
    *
    * Example:
    * {{{
    *   lineItem.bimap(_.name)((item, value) => item.copy(name = value))
    * }}}
    */
  def bimap[PartOfA](
    get: A => PartOfA
  )(set: UpdatableSignal.Setter[A, PartOfA]): UpdatableSignal[PartOfA] =
    UpdatableSignal[PartOfA](
      signal.map(get),
      () => get(now()),
      partOfA => {
        val a = now()
        val newA = set(a, partOfA)
        setTo(newA)
      },
    )

  /** Returns a new [[UpdatableSignal]] which is constructed from a [[Signal]] (obtained via one of the
    * [[SplittableSignal.split]] methods) and an update function.
    *
    * Example:
    * {{{
    *   val lineItems: UpdatableSignal[NonEmptyVector[LineItem]] = ...
    *
    *   div(
    *     children <-- lineItems.signal.splitByIndex((idx, _, signal) =>
    *       LineItem(
    *         lineItems.bimapFromSplit(signal)(_.updatedWith(idx, _)),
    *       )
    *     )
    *   )
    * }}}
    *
    * @param signal
    *   the [[Signal]] that represents a part of [[A]], usually obtained via one of the `split` methods.
    * @param update
    *   the function that updates the underlying data source. The first argument is the current value of
    *   [[self.signal]], the second argument is a function that updates the given part of [[A]].
    */
  // TODO: this probably needs a better name.
  def bimapFromSplit[PartOfA](signal: Signal[PartOfA])(
    get: A => PartOfA,
    update: UpdatableSignal.Setter[A, PartOfA],
  ): UpdatableSignal[PartOfA] = UpdatableSignal[PartOfA](
    signal,
    () => get(now()),
    partOfA => {
      val a = now()
      val newA = update(a, partOfA)
      setTo(newA)
    },
  )
}
object UpdatableSignal {

  /** Sets [[PartOfA]] in [[A]] and returns the modified [[A]]. */
  // noinspection ScalaWeakerAccess
  type Setter[A, PartOfA] = (A, PartOfA) => A

  /** Creates an instance from a [[Signal]] and [[UpdateFn]]. */
  def apply[A](
    signal: Signal[A],
    now: () => A,
    setTo: A => Unit,
  ): UpdatableSignal[A] = {
    val s = signal
    val n = now
    val set = setTo

    new UpdatableSignal[A] {
      override def signal: Signal[A] = s
      override def now(): A = n()
      override def setTo(value: A): Unit = set(value)
    }
  }

  /** Creates an instance from a [[Var]] and mapping functions. */
  given fromVar[A]: Conversion[Var[A], UpdatableSignal[A]] = rxVar =>
    UpdatableSignal[A](rxVar.signal, rxVar.now, rxVar.set)

  extension [A](s: UpdatableSignal[NonEmptyVector[A]]) {

    /** A version of [[UpdatableSignal.bimapFromSplit]] for [[NonEmptyVector]]s.
      *
      * Example:
      * {{{
      * def render(goodsEntries: UpdatableSignal[NonEmptyVector[GoodsEntry]]) = {
      *   val tableRows = goodsEntries.signal
      *     .map(_.toVector)
      *     .splitByIndex { (idx, _, signal) =>
      *       val updatableSignal = goodsEntries.zoomAfterSplitByIndex(signal, idx)
      *     }
      * }
      * }}}
      *
      * @param signal
      *   the [[Signal]] for a particular index in the vector obtained via `splitByIndex`.
      * @param idx
      *   the index in the vector of that element obtained via `splitByIndex`.
      */
    def zoomAfterSplitByIndex(signal: Signal[A], idx: Int): UpdatableSignal[A] = UpdatableSignal[A](
      signal,
      () => s.now().getUnsafe(idx),
      a => {
        val vector = s.now()
        val newVector = vector.updatedUnsafe(idx, a)
        s.setTo(newVector)
      },
    )
  }
}
