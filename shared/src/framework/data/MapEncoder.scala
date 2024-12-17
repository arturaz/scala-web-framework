package framework.data

import cats.InvariantSemigroupal
import cats.ContravariantSemigroupal
import cats.Contravariant

/** Encodes values as [[Map]]s.
  *
  * Example:
  * {{{
  * case class Person(id: Int, name: String)
  *
  * val personEncoder = MapEncoder.ofParts(
  *   MapEncoder.PartEncoder.at("id")((_: Person).id.toString),
  *   MapEncoder.PartEncoder.at("name")(_.name)
  * )
  *
  * personEncoder.encode(Person(123, "John"))
  * }}}
  */
trait MapEncoder[K, +V, -Input] {
  def encode(input: Input): Map[K, V]

  def contramap[OtherInput](f: OtherInput => Input): MapEncoder[K, V, OtherInput] =
    input => encode(f(input))
}
object MapEncoder {

  /** Slightly more efficient version of [[MapEncoder]] that encodes a single value into the ([[K]], [[V]]) pair. */
  trait PartEncoder[K, +V, -Input] {
    def encode(input: Input): (K, V)

    def contramap[OtherInput](f: OtherInput => Input): PartEncoder[K, V, OtherInput] =
      input => encode(f(input))

    def asMapEncoder: MapEncoder[K, V, Input] = input => Map(encode(input))
  }
  object PartEncoder {

    /** Encodes a single value into the map at the specified key. */
    def at[K, V, Input](key: K)(encode: Input => V): PartEncoder[K, V, Input] = v => key -> encode(v)

    /** Encodes a single value into the map at the specified key without encode function. */
    def atPure[K, V](key: K): PartEncoder[K, V, V] = v => key -> v

    given asMapEncoder[K, V, Input]: Conversion[PartEncoder[K, V, Input], MapEncoder[K, V, Input]] = _.asMapEncoder

    given contravariant[K, V]: Contravariant[[Input] =>> PartEncoder[K, V, Input]] = new {

      override def contramap[A, B](fa: PartEncoder[K, V, A])(f: B => A): PartEncoder[K, V, B] = fa.contramap(f)

    }
  }

  /** Encodes a single value into the map at the specified key. */
  def at[K, V, Input](key: K)(encode: Input => V): MapEncoder[K, V, Input] = v => Map(key -> encode(v))

  def ofParts[K, V, Input](encoders: PartEncoder[K, V, Input]*): MapEncoder[K, V, Input] = v => {
    encoders.iterator.map(_.encode(v)).toMap
  }

  def of[K, V, Input](encoders: MapEncoder[K, V, Input]*): MapEncoder[K, V, Input] = v => {
    encoders.foldLeft(Map.empty[K, V]) { case (acc, encoder) => acc ++ encoder.encode(v) }
  }

  given contravariantSemigroupal[K, V]: ContravariantSemigroupal[[X] =>> MapEncoder[K, V, X]] = new {

    override def contramap[A, B](fa: MapEncoder[K, V, A])(f: B => A): MapEncoder[K, V, B] =
      fa.contramap(f)

    override def product[A, B](fa: MapEncoder[K, V, A], fb: MapEncoder[K, V, B]): MapEncoder[K, V, (A, B)] = {
      case (a, b) => fa.encode(a) ++ fb.encode(b)
    }
  }
}
