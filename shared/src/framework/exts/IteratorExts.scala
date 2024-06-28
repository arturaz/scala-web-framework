package framework.exts

import collection.mutable

extension [A](iterator: Iterator[A]) {

  /** Inserts a separator between the elements of the iterator. Returns an empty iterator if the source iterator is
    * empty.
    *
    * TODO: test
    */
  def intersperse[B >: A](b: B): Iterator[B] = {
    if (iterator.isEmpty) Iterator.empty
    else {
      val first = iterator.next()

      Iterator(first) ++ iterator.flatMap(a => Iterator(b, a))
    }
  }

  /** Returns true if all of the values in the iterator are distinct by the given function.
    *
    * TODO: test
    */
  def allDistinctBy[B](f: A => B)(using CanEqual[B, B]): Boolean = {
    val seen = mutable.Set.empty[B]
    while (iterator.hasNext) {
      val a = iterator.next()
      val b = f(a)

      if (seen.contains(b)) return false
      else seen += b
    }

    true
  }

  /** Returns true if all of the values in the iterator are same by the given function.
    *
    * TODO: test
    */
  def allSameBy[B](f: A => B)(using CanEqual[B, B]): Boolean = {
    var seen = Option.empty[B]
    while (iterator.hasNext) {
      val a = iterator.next()
      val b = f(a)

      seen match {
        case None                => seen = Some(b)
        case Some(b2) if b != b2 => return false
        case Some(_)             =>
      }
    }

    true
  }
}
