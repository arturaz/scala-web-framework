package framework.utils

/** A type-class that indicates that the type can get a next element. */
trait Successor[A] {
  def next(a: A): A
}
object Successor {
  given Successor[Byte] = b => (b + 1).toByte
  given Successor[Short] = s => (s + 1).toShort
  given Successor[Int] = _ + 1
  given Successor[Long] = _ + 1
  given Successor[BigInt] = _ + 1
}
