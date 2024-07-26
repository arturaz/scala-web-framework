package framework.data

import cats.syntax.all.*
import framework.utils.Validatable
import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.partial.Result.Errors
import org.scalajs.dom.File
import scala.scalajs.js.typedarray.ArrayBuffer

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
case class FormFileHolder(
  maybeFile: Option[File],
  maxLengthInBytes: Option[Long],
  acceptableExtensions: Option[NonEmptySet[String]] = None,
  validation: File => Option[Errors],
) {
  def withFile(file: Option[File]) = copy(maybeFile = file)

  /** The value for the input `accept` attribute. */
  def acceptAttributeValue: String = acceptableExtensions.fold("*")(exts => exts.toSortedSet.mkString(", "))
}
object FormFileHolder {
  def empty(
    maxLengthInBytes: Option[Long] = None,
    acceptableExtensions: Option[NonEmptySet[String]] = None,
    validation: File => Option[Errors] = _ => None,
  ): FormFileHolder = apply(None, maxLengthInBytes, acceptableExtensions, validation)

  given partialTransformer: PartialTransformer[FormFileHolder, FormFile] = PartialTransformer { holder =>
    holder.maybeFile match
      case None => Result.fromErrorString("No file selected.")
      case Some(file) =>
        val lengthErrors = holder.maxLengthInBytes.flatMap { maxLength =>
          Option.when(file.size > maxLength)(
            Errors.fromString(s"File is too large, maximum size is ${maxLength.toBytesPretty}.")
          )
        }

        val extensionErrors = holder.acceptableExtensions.flatMap { acceptableExtensions =>
          val hasValidExtension = acceptableExtensions.exists(ext => file.name.endsWith(ext))

          Option.unless(hasValidExtension)(
            Errors.fromString(show"Only ${acceptableExtensions.toSortedSet.mkString(", ")} files are allowed.")
          )
        }

        val validationErrors = holder.validation(file)

        val errors = lengthErrors |+| extensionErrors |+| validationErrors

        Result.fromEither(errors.toLeft(FormFile(file)))
  }

  given Validatable[FormFileHolder] = Validatable.fromPartialTransformer(partialTransformer)
}
