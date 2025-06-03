package framework.exts

extension (a: js.Dynamic) {

  /** Tries to access a member, if it's not `undefined` casts it to the specified type. */
  def ifDefined[A <: js.Any](f: js.Dynamic => js.Dynamic): Option[A] = {
    val selected = f(a)
    if (js.isUndefined(selected)) None else Some(selected.asInstanceOf[A])
  }
}
