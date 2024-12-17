package framework.data

import cats.data.NonEmptyChain
import cats.InvariantMonoidal

/** Encodes and decodes values as [[Map]]s.
  *
  * Example:
  * {{{
  * case class Person(id: Int, name: String)
  *
  * val personCodec = (
  *   MapCodec.at("id")((_: String).toIntOption.toRight("not an int"), _.toString),
  *   MapCodec.at("name")((s: String) => Right(s), identity)
  * ).imapN(Person.apply)(Tuple.fromProductTyped)
  * }}}
  */
trait MapCodec[K, V, A] extends MapEncoder[K, V, A] with MapDecoder[K, V, A] { self =>
  def imap[B](f: A => B)(g: B => A): MapCodec[K, V, B] = new {
    override def encode(input: B): Map[K, V] = self.encode(g(input))

    override def decode(map: Map[K, V]): Either[NonEmptyChain[MapDecoder.DecodeFailure[K, V]], B] =
      self.decode(map).map(f)
  }
}
object MapCodec {
  trait PartCodec[K, V, A] extends MapDecoder.PartDecoder[K, V, A] with MapEncoder.PartEncoder[K, V, A] { self =>
    def asCodec: MapCodec[K, V, A] = MapCodec.fromUsing(using this.asMapDecoder, this.asMapEncoder)

    def imap[B](f: A => B)(g: B => A): PartCodec[K, V, B] = new {
      override def encode(input: B): (K, V) = self.encode(g(input))

      override def decode(map: Map[K, V]): Either[MapDecoder.DecodeFailure[K, V], B] =
        self.decode(map).map(f)
    }
  }
  object PartCodec {
    given asCodec[K, V, A]: Conversion[PartCodec[K, V, A], MapCodec[K, V, A]] = _.asCodec

    given fromUsing[K, V, A](using
      decoder: MapDecoder.PartDecoder[K, V, A],
      encoder: MapEncoder.PartEncoder[K, V, A],
    ): PartCodec[K, V, A] =
      new {
        override def decode(map: Map[K, V]): Either[MapDecoder.DecodeFailure[K, V], A] = decoder.decode(map)

        override def encode(input: A): (K, V) = encoder.encode(input)
      }

    /** Encodes and decodes a single value from the map at the specified key. */
    def at[K, V, A](key: K)(decode: V => Either[String, A])(encode: A => V): PartCodec[K, V, A] =
      fromUsing(using MapDecoder.PartDecoder.at(key)(decode), MapEncoder.PartEncoder.at(key)(encode))

    /** Encodes and decodes a single value from the map at the specified key without decode/encode functions. */
    def atPure[K, V](key: K): PartCodec[K, V, V] =
      at[K, V, V](key)(v => Right(v))(identity)

    given invariant[K, V]: cats.Invariant[[A] =>> PartCodec[K, V, A]] = new {
      override def imap[A, B](fa: PartCodec[K, V, A])(f: A => B)(g: B => A): PartCodec[K, V, B] =
        fa.imap(f)(g)
    }
  }

  /** Encodes and decodes a single value from the map at the specified key. */
  def at[K, V, A](key: K)(decode: V => Either[String, A])(encode: A => V): MapCodec[K, V, A] =
    PartCodec.at(key)(decode)(encode).asCodec

  given fromUsing[K, V, A](using
    decoder: MapDecoder[K, V, A],
    encoder: MapEncoder[K, V, A],
  ): MapCodec[K, V, A] = new MapCodec[K, V, A] {
    override def decode(map: Map[K, V]) = decoder.decode(map)

    override def encode(input: A) = encoder.encode(input)
  }

  /** Provides a [[MapCodec]] for [[Unit]] values. */
  def Unit[K, V]: MapCodec[K, V, Unit] = new {
    override def encode(input: Unit): Map[K, V] = Map.empty

    override def decode(map: Map[K, V]): Either[NonEmptyChain[MapDecoder.DecodeFailure[K, V]], Unit] = Right(())
  }

  given invariantMonoidal[K, V]: InvariantMonoidal[[A] =>> MapCodec[K, V, A]] = new {

    override def unit: MapCodec[K, V, Unit] = MapCodec.Unit

    override def imap[A, B](fa: MapCodec[K, V, A])(f: A => B)(g: B => A): MapCodec[K, V, B] =
      fa.imap(f)(g)

    override def product[A, B](fa: MapCodec[K, V, A], fb: MapCodec[K, V, B]): MapCodec[K, V, (A, B)] =
      new {
        override def encode(input: (A, B)): Map[K, V] =
          fa.encode(input._1) ++ fb.encode(input._2)

        override def decode(map: Map[K, V]): Either[NonEmptyChain[MapDecoder.DecodeFailure[K, V]], (A, B)] =
          for {
            a <- fa.decode(map)
            b <- fb.decode(map)
          } yield (a, b)
      }
  }
}
