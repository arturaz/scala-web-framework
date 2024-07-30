package framework.utils

import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.partial.Result
import io.scalaland.chimney.partial.Result.Errors

/** Interface for types that can be validated. */
trait Validatable[-A] {
  def isValid(value: A): Boolean = validate(value).isEmpty

  def validate(value: A): Option[Result.Errors]
}
object Validatable {
  def of[A](validate: A => Option[Result.Errors]): Validatable[A] = {
    val v = validate
    new Validatable[A] {
      override def validate(value: A): Option[Errors] = v(value)
    }
  }

  def fromPartialTransformer[A](t: PartialTransformer[A, ?]): Validatable[A] =
    of(t.transform(_).asEither.left.toOption)

  /** Always passes validation. */
  val alwaysValid: Validatable[Any] = of(_ => None)
}
