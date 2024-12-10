package framework.utils

import com.raquo.airstream.split.SplittableSignal
import monocle.macros.GenLens
import monocle.Lens
import monocle.Focus.KeywordContext
import monocle.Focus.MkFocus

/** Like [[Var.zoom]] but is not required to have an [[com.raquo.airstream.ownership.Owner]].
  *
  * @note
  *   This probably became unnecessary after https://laminar.dev/blog/2024/08/14/laminar-v17.1.0#splitting-var-s.
  */
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

  /** Returns a new [[UpdatableSignal]] which is mapped using a [[Lens]]. */
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
  def bimapGenLens[PartOfA](f: MkFocus[A] => Lens[A, PartOfA]): UpdatableSignal[PartOfA] = {
    val lens = f(GenLens[A])
    bimap(lens)
  }

  /** Returns a new [[UpdatableSignal]] for the [[PartOfA]] after you invoke `splitMatchOne`.
    *
    * Example:
    * {{{
    *  updatableSignal.signal.splitMatchOne
    *    .handle { (_, partSignal) => // handle enum case A
    *      val partUpdatableSignal = updatableSignal.zoomAfterSplitMatchOne(partSignal)
    *      ...
    *    }
    *    .handle { (_, partSignal) => // handle enum case B
    *      ...
    *    }
    *    .close
    * }}}
    */
  def zoomAfterSplitMatchOne[PartOfA <: A](signal: Signal[PartOfA]): UpdatableSignal[PartOfA] =
    UpdatableSignal(
      signal,
      () => self.now().asInstanceOf[PartOfA],
      self.setTo,
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

    /** Example:
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
