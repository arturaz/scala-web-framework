package framework.exts

import io.scalaland.chimney.{partial, PartialTransformer, Transformer}
import io.circe.*
import cats.syntax.all.*

implicit class PartialTransformerExts[From, To](t: PartialTransformer[From, To]) extends AnyVal {
  def andThen[To2](t2: PartialTransformer[To, To2]): PartialTransformer[From, To2] =
    PartialTransformer { from =>
      t.transform(from).flatMap(t2.transform)
    }

  def andThen[To2](t2: Transformer[To, To2]): PartialTransformer[From, To2] =
    PartialTransformer { from =>
      t.transform(from).map(t2.transform)
    }
}

extension (obj: PartialTransformer.type) {
  def fromEitherString[From, To](f: From => Either[String, To]): PartialTransformer[From, To] =
    PartialTransformer(from => partial.Result.fromEitherString(f(from)))

  def fromEitherStrings[From, To](f: From => Either[Seq[String], To]): PartialTransformer[From, To] =
    PartialTransformer(from =>
      partial.Result.fromEither(f(from).left.map(seq => partial.Result.Errors.fromStrings(seq.head, seq.tail*)))
    )

  /** Creates a [[PartialTransformer]] from a Circe [[Decoder]]. */
  def fromJsonString[To](using decoder: Decoder[To]): PartialTransformer[String, To] =
    fromEitherString(from => io.circe.parser.parse(from).leftMap(_.show).flatMap(_.as[To].leftMap(_.show)))

  /** Creates a [[PartialTransformer]] from a Circe [[Decoder]]. */
  def fromJson[To](using decoder: Decoder[To]): PartialTransformer[Json, To] =
    fromEitherString(from => from.as[To].leftMap(_.show))
}

extension (obj: partial.Result.type) {
  def fromMaybeErrorStrings[Value](value: => Value, errors: String*): partial.Result[Value] =
    if (errors.isEmpty) partial.Result.Value(value) else partial.Result.Errors.fromStrings(errors.head, errors.tail*)
}
