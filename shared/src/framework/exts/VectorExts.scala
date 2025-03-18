package framework.exts

implicit class VectorExts[A](private val vector: Vector[A]) extends AnyVal {

  /** Removes the element at the given index. */
  def removeAt(idx: Int): Vector[A] = {
    val (before, after) = vector.splitAt(idx)
    before ++ after.tail
  }

  /** Replaces the element at the given index. */
  def replaceAt(idx: Int, elem: A): Vector[A] = {
    val (before, after) = vector.splitAt(idx)
    (before :+ elem) ++ after.tail
  }
}
