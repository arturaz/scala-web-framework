package framework.data

import cats.syntax.all.*
import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.partial.Result.Errors
import org.scalajs.dom.File
import scala.scalajs.js.typedarray.ArrayBuffer
import yantl.Validator
import yantl.ValidatorRule

/** A validated file in the form. */
case class FormFile(file: File) {
  def toArrayBufferIO: IO[ArrayBuffer] =
    IO.fromPromise(IO(file.arrayBuffer()))

  def toByteArrayIO: IO[Array[Byte]] =
    toArrayBufferIO.map(_.toByteArray)

  def toByteIArrayIO: IO[IArray[Byte]] =
    toArrayBufferIO.map(_.toByteIArray)
}

/** A holder for a file in the form which defines the validation logic.
  *
  * @param maybeFile
  *   the file if it is present
  * @param maxLengthInBytes
  *   the maximum length of the file in bytes
  * @param acceptableExtensions
  *   the acceptable file extensions (for example '.png' or '.jpg')
  * @param validation
  *   extra validation logic
  */
case class FormFileHolder[+TError](
  maybeFile: Option[File],
  maxLengthInBytes: Option[Long],
  acceptableExtensions: Option[NonEmptySet[String]] = None,
  validation: Validator[File, TError] = Validator.noOp,
) {
  def withFile(file: Option[File]) = copy(maybeFile = file)

  /** The value for the input `accept` attribute. */
  def acceptAttributeValue: String = acceptableExtensions.fold("*")(exts => exts.toSortedSet.mkString(", "))
}
object FormFileHolder {
  type TError = FileTooLarge | InvalidFileExtension

  def empty[TError](
    maxLengthInBytes: Option[Long] = None,
    acceptableExtensions: Option[NonEmptySet[String]] = None,
    validation: Validator[File, TError] = Validator.noOp,
  ): FormFileHolder[TError] = apply(None, maxLengthInBytes, acceptableExtensions, validation)

  case class FileTooLarge(maxLengthInBytes: Long, file: File) derives CanEqual {
    override def toString(): String =
      s"Cannot be larger than $maxLengthInBytes ${ValidatorRule.English.bytes(maxLengthInBytes)}, " +
        s"was ${file.size} ${ValidatorRule.English.bytes(file.size.toLong)}."
  }

  case class InvalidFileExtension(acceptableExtensions: NonEmptySet[String], file: File) derives CanEqual {
    override def toString(): String =
      s"File must have one of the following extensions: ${acceptableExtensions.toSortedSet.mkString(", ")}, " +
        s"however the file name was '${file.name}'."
  }

  given partialTransformer[TError]: PartialTransformer[FormFileHolder[TError], FormFile] = PartialTransformer {
    holder =>
      holder.maybeFile match {
        case None => Result.fromErrorString("No file selected.")
        case Some(file) =>
          Result.fromMaybeErrorStrings(FormFile(file), validator.validate(holder).map(_.toString)*)
      }
  }

  val validatorRuleMaxLengthInBytes: ValidatorRule[FormFileHolder[?], FileTooLarge] =
    ValidatorRule.of { holder =>
      for {
        file <- holder.maybeFile
        maxLength <- holder.maxLengthInBytes
        error <- Option.when(file.size > maxLength)(FileTooLarge(maxLength, file))
      } yield error
    }

  val validatorRuleAcceptableExtensions: ValidatorRule[FormFileHolder[?], InvalidFileExtension] =
    ValidatorRule.of { holder =>
      for {
        file <- holder.maybeFile
        acceptableExtensions <- holder.acceptableExtensions
        error <- Option.when(!acceptableExtensions.exists(ext => file.name.endsWith(ext)))(
          InvalidFileExtension(acceptableExtensions, file)
        )
      } yield error
    }

  given validator[TError]: Validator[FormFileHolder[TError], TError | FileTooLarge | InvalidFileExtension] =
    Validator.of { holder =>
      holder.maybeFile.flatMap(holder.validation.validate).toVector ++
        validatorRuleMaxLengthInBytes.validate(holder).toVector ++
        validatorRuleAcceptableExtensions.validate(holder).toVector
    }
}
