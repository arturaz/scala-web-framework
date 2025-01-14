package framework.data

import alleycats.Empty
import framework.exts.*
import framework.prelude.*
import io.scalaland.chimney.{PartialTransformer, Transformer}
import scala.reflect.ClassTag

/** Data for an autocompletable field. */
enum AutocompleteData[+A] {

  /** The raw input from the user. */
  case RawInput(input: String)

  /** The user has selected an item from the options. */
  case SelectedItem(item: A)

  def asInput: Option[String] = this match {
    case RawInput(input)    => Some(input)
    case SelectedItem(item) => None
  }

  def asItem: Option[A] = this match {
    case SelectedItem(item) => Some(item)
    case RawInput(_)        => None
  }
}
object AutocompleteData {
  given empty[A]: Empty[AutocompleteData[A]] = Empty(RawInput(""))

  given circeCodec[A: CirceEncoder: CirceDecoder]: CirceCodec[AutocompleteData[A]] = CirceCodec.derived

  given transformerToString[A](using aToString: Transformer[A, String]): Transformer[AutocompleteData[A], String] = {
    case RawInput(input)    => input
    case SelectedItem(item) => aToString.transform(item)
  }

  given transformerToOptionOfA[A]: Transformer[AutocompleteData[A], Option[A]] = {
    case RawInput(_)        => None
    case SelectedItem(item) => Some(item)
  }

  given partialTransformerToA[A](using ct: ClassTag[A]): PartialTransformer[AutocompleteData[A], A] =
    PartialTransformer.fromEitherString {
      case SelectedItem(item) => Right(item)
      case v: RawInput[_]     => Left(s"Can't extract value from $v to make a '${ct.runtimeClass.getName}'")
    }

  given transformerToOptionOfB[A: ClassTag, B](using
    aToB: Transformer[A, B]
  ): Transformer[AutocompleteData[A], Option[B]] =
    transformerToOptionOfA[A].andThen((opt: Option[A]) => opt.map(aToB.transform))

  given partialTransformerToB[A: ClassTag, B](using
    aToB: PartialTransformer[A, B]
  ): PartialTransformer[AutocompleteData[A], B] =
    partialTransformerToA[A].andThen(aToB)
}
