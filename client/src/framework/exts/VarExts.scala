package framework.exts

import monocle.Focus.MkFocus

extension [A](rxVar: Var[A]) {

  /** Returns a new [[Var]] which is zoomed using a [[Lens]]. */
  def zoomLazyLens[PartOfA](lens: Lens[A, PartOfA]): Var[PartOfA] =
    rxVar.zoomLazy(lens.get)((a, part) => lens.replace(part)(a))

  /** Does the zooming with a macro, generating the lens from the function `f`.
    *
    * Example:
    * {{{
    *   lineItem.zoomLazyGenLens(_(_.name))
    * }}}
    *
    * or
    *
    * {{{
    *   lineItem.zoomLazyGenLens(mk => mk(_.name))
    * }}}
    */
  def zoomLazyGenLens[PartOfA](f: MkFocus[A] => Lens[A, PartOfA]): Var[PartOfA] = {
    val lens = f(GenLens[A])
    rxVar.zoomLazyLens(lens)
  }
}
