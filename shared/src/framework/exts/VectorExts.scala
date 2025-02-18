package framework.exts

implicit class VectorExts[A](private val vector: Vector[A]) extends AnyVal {

  /** Removes the element at the given index. */
  def removeAt(idx: Int): Vector[A] = {
    val (before, after) = vector.splitAt(idx)
    before ++ after.drop(1)
  }
}
