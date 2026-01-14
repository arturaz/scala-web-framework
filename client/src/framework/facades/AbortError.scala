package framework.facades

/** Indicates that a network request has been aborted. */
case object AbortError derives CanEqual {

  /** Checks if this value is an [[AbortError]]. */
  def is(any: Any): Boolean = any match {
    case obj: scalajs.js.Object => obj.asDynamic.name == "AbortError"
    case _                      => false
  }

  def unapply(any: Any): Option[AbortError.type] = if (is(any)) Some(AbortError) else None
}
