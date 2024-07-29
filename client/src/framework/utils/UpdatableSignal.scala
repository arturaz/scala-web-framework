package framework.utils

import com.raquo.airstream.split.SplittableSignal
import monocle.macros.GenLens
import monocle.Lens
import monocle.Focus.KeywordContext
import monocle.Focus.MkFocus

/** Like [[Var.zoom]] but is not required to have an [[com.raquo.airstream.ownership.Owner]]. */
trait UpdatableSignal[A] extends ZoomedOwnerlessSignal[A] { self =>

  /** The current value. */
  def now(): A

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
  )(set: UpdatableSignal.PartSetter[A, PartOfA]): UpdatableSignal[PartOfA] =
    bimap(Lens(get)(partOfA => a => set(a, partOfA)))

  /** Returns a new [[UpdatableSignal]] which is mapped.
    *
    * Example:
    * {{{
    *   lineItem.bimap(_.name)((item, value) => item.copy(name = value))
    * }}}
    */
  def bimap[PartOfA](lens: Lens[A, PartOfA]): UpdatableSignal[PartOfA] =
    UpdatableSignal[PartOfA](
      signal.map(lens.get),
      () => lens.get(now()),
      partOfA => {
        val a = now()
        val newA = lens.replace(partOfA)(a)
        setTo(newA)
      },
    )

  /** Does the mapping with a macro, generating the lens from the function `f`.
    *
    * Example:
    * {{{
    *   lineItem.bimapGenLens(_(_.name))
    * }}}
    *
    * or
    *
    * {{{
    *   lineItem.bimapGenLens(mk => mk(_.name))
    * }}}
    */
  inline def bimapGenLens[PartOfA](inline f: MkFocus[A] => Lens[A, PartOfA]): UpdatableSignal[PartOfA] = {
    val lens = f(GenLens[A])
    bimap(lens)
  }

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
    update: UpdatableSignal.PartSetter[A, PartOfA],
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
  type PartSetter[A, PartOfA] = (A, PartOfA) => A

  /** Creates an instance. */
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

  given fromVar[A]: Conversion[Var[A], UpdatableSignal[A]] = rxVar => apply(rxVar.signal, rxVar.now, rxVar.set)
  given fromPersistedVar[A]: Conversion[PersistedVar[A], UpdatableSignal[A]] = rxVar => fromVar(rxVar.underlying)

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
