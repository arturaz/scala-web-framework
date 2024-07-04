package framework.exts

extension (t: Throwable) {

  /** Returns a [[List]] of all the causes of the [[Throwable]]. */
  def causes: List[Throwable] = {
    val causes = scala.collection.mutable.ListBuffer[Throwable](t)
    var current = t
    while (current.getCause != null) {
      current = current.getCause
      causes += current
    }
    causes.toList
  }

  /** Renders this [[Throwable]] and it's causes along with all stacktraces as a string. */
  def errorWithCausesAndStacktracesString: String = {
    val errors = t :: t.causes

    errors.iterator
      .map { err =>
        s"""|$err
            |  ### Stacktrace ###
            |  # ${err.getStackTrace().mkString("\n  # ")}
            |""".stripMargin
      }
      .mkString("\n  caused by\n")
  }
}
