package framework.exts

extension [A](a: A | Null) {

  /** Converts a type union to an option. */
  def jsValueOrNullToOption: Option[A] = {
    if (a.asInstanceOf[AnyRef] eq null) None
    else Some(a.asInstanceOf[A])
  }
}
