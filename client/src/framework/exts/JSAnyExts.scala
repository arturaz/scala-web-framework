package framework.exts

extension (a: js.Any) {

  /** Types the value as [[js.Dynamic]]. */
  def asDynamic: js.Dynamic = a.asInstanceOf
}
