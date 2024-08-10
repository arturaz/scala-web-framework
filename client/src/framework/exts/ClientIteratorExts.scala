package framework.exts

extension [A](iterator: Iterator[A]) {
  def toJsArray: js.Array[A] = {
    val arr = js.Array[A]()
    iterator.foreach(arr.push(_))
    arr
  }
}
