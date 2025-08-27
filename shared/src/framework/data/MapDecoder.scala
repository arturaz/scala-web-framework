package framework.data

import cats.Applicative
import cats.data.NonEmptyChain

/** Decodes values from [[Map]]s.
  *
  * Example:
  * {{{
  * // `decoder` is a `cats.Applicative`, so all of the applicative operations are available.
  * val decoder = (
  *   MapDecoder.at("a")((_: String).toIntOption.toRight("not an int")),
  *   MapDecoder.at("b")((_: String).toLongOption.toRight("not a long"))
  * ).tupled
  * }}}
  */
trait MapDecoder[K, V, +Output] {
  def decode(map: Map[K, V]): Either[NonEmptyChain[MapDecoder.DecodeFailure[K, V]], Output]

  def map[B](f: Output => B): MapDecoder[K, V, B] =
    map => decode(map).map(f)

  def flatMap[B](f: Output => Either[NonEmptyChain[MapDecoder.DecodeFailure[K, V]], B]): MapDecoder[K, V, B] =
    map => decode(map).flatMap(f)

  def flatMapDecoder[B](f: Output => MapDecoder[K, V, B]): MapDecoder[K, V, B] =
    map => decode(map).flatMap(output => f(output).decode(map))
}
object MapDecoder {
  enum DecodeFailure[+K, +V] {
    case MissingKey(key: K)
    case InvalidValue(key: K, value: V, message: String)
  }

  /** Decodes a single value from the map. */
  trait PartDecoder[K, V, +Output] {
    def decode(map: Map[K, V]): Either[DecodeFailure[K, V], Output]

    def map[B](f: Output => B): PartDecoder[K, V, B] =
      map => decode(map).map(f)

    def flatMap[B](f: Output => Either[DecodeFailure[K, V], B]): PartDecoder[K, V, B] =
      map => decode(map).flatMap(f)

    def flatMapDecoder[B](f: Output => PartDecoder[K, V, B]): PartDecoder[K, V, B] =
      map => decode(map).flatMap(output => f(output).decode(map))

    /** Converts this [[PartDecoder]] to a [[MapDecoder]]. */
    def asMapDecoder: MapDecoder[K, V, Output] = map => decode(map).left.map(NonEmptyChain.one)
  }
  object PartDecoder {

    /** Decodes a single value from the map at the specified key. */
    def at[K, V, Output](key: K)(decode: V => Either[String, Output]): PartDecoder[K, V, Output] = { map =>
      map.get(key) match {
        case None        => Left(DecodeFailure.MissingKey(key))
        case Some(value) => decode(value).left.map(err => DecodeFailure.InvalidValue(key, value, err))
      }
    }

    /** Decodes a single value from the map at the specified key without decode function. */
    def atPure[K, V](key: K): PartDecoder[K, V, V] = at(key)(v => Right(v))

    given asMapDecoder[K, V, Output]: Conversion[PartDecoder[K, V, Output], MapDecoder[K, V, Output]] = _.asMapDecoder

    given applicative[K, V]: Applicative[[Output] =>> PartDecoder[K, V, Output]] = new {
      override def pure[A](x: A): PartDecoder[K, V, A] = _ => Right(x)

      override def ap[A, B](ff: PartDecoder[K, V, A => B])(fa: PartDecoder[K, V, A]): PartDecoder[K, V, B] =
        map => {
          for {
            a <- fa.decode(map)
            f <- ff.decode(map)
          } yield f(a)
        }
    }
  }

  /** Decodes a single value from the map at the specified key. */
  def at[K, V, Output](key: K)(decode: V => Either[String, Output]): MapDecoder[K, V, Output] =
    PartDecoder.at(key)(decode).asMapDecoder

  given applicative[K, V]: Applicative[[Output] =>> MapDecoder[K, V, Output]] = new {
    override def pure[A](x: A): MapDecoder[K, V, A] = _ => Right(x)

    override def ap[A, B](ff: MapDecoder[K, V, A => B])(fa: MapDecoder[K, V, A]): MapDecoder[K, V, B] = { map =>
      {
        val aEither = fa.decode(map)
        val fEither = ff.decode(map)

        (aEither, fEither) match {
          case (Left(aFail), Left(fFail)) => Left(fFail ++ aFail)
          case (Left(aFail), Right(_))    => Left(aFail)
          case (Right(_), Left(fFail))    => Left(fFail)
          case (Right(a), Right(f))       => Right(f(a))
        }
      }
    }
  }
}
