package framework.utils

import scala.util.Try
import cats.Invariant
import framework.prelude.{CirceKeyDecoder, CirceKeyEncoder}

/** A helper to provide both [[CirceKeyDecoder]] and [[CirceKeyEncoder]]. */
trait CirceKeyCodec[A] extends CirceKeyDecoder[A] with CirceKeyEncoder[A] { self =>

  /** A two-way mapping when it can fail. Similar to [[CircleCodec.iemap]]. */
  def iomap[B](f: A => Option[B])(g: B => A): CirceKeyCodec[B] = new CirceKeyCodec[B] {
    override def apply(key: String): Option[B] = self(key).flatMap(f)
    override def apply(key: B): String = self(g(key))
  }
}
object CirceKeyCodec {
  def instance[A](decode: String => Option[A])(encode: A => String): CirceKeyCodec[A] =
    new CirceKeyCodec[A] {
      override def apply(key: String): Option[A] = decode(key)
      override def apply(key: A): String = encode(key)
    }

  /** A [[CirceKeyCodec]] that is defined in terms of [[Int]] ordinal. Can be conveniently defined for Scala 3 `enum`s.
    *
    * {{{
    * enum Foo { case A, B, C }
    * object Foo {
    *   given CirceKeyCodec[Foo] = CirceKeyCodec.fromOrdinal(fromOrdinal)(_.ordinal)
    * }
    * }}}
    *
    * @see
    *   [[forEnum]]
    */
  def fromOrdinal[A](fromOrdinal: Int => A)(toOrdinal: A => Int): CirceKeyCodec[A] =
    apply[Int].iomap(ordinal => Try(fromOrdinal(ordinal)).toOption)(toOrdinal)

  /** A [[CirceKeyCodec]] that is defined for Scala 3 `enum`s.
    *
    * {{{
    * enum Foo { case A, B, C }
    * object Foo {
    *   given CirceKeyCodec[Foo] = CirceKeyCodec.forEnum(fromOrdinal)
    * }
    * }}}
    *
    * @see
    *   [[fromOrdinal]]
    */
  def forEnum[A <: reflect.Enum](fromOrdinal: Int => A): CirceKeyCodec[A] =
    CirceKeyCodec.fromOrdinal(fromOrdinal)(_.ordinal)

  implicit def apply[A](implicit dec: CirceKeyDecoder[A], enc: CirceKeyEncoder[A]): CirceKeyCodec[A] =
    new CirceKeyCodec[A] {
      override def apply(key: String): Option[A] = dec(key)
      override def apply(key: A): String = enc(key)
    }

  given invariant: Invariant[CirceKeyCodec] with
    override def imap[A, B](codec: CirceKeyCodec[A])(f: A => B)(g: B => A): CirceKeyCodec[B] =
      new CirceKeyCodec[B] {
        override def apply(key: String): Option[B] = codec(key).map(f)
        override def apply(key: B): String = codec(g(key))
      }
}
