package framework.utils

/** Converts a value to its ordinal. */
trait ToOrdinal[A] {
  def ordinal(a: A): Int
}
object ToOrdinal {
  extension [A](a: A)(using toOrdinal: ToOrdinal[A]) {
    def ordinal: Int = toOrdinal.ordinal(a)
  }
}
