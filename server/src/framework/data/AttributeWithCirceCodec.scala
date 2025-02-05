package framework.data

import org.typelevel.otel4s.Attribute
import io.circe.Json

final case class AttributeWithCirceEncoder[A](
  attribute: Attribute[A],
  attributeEncoder: CirceEncoder[Attribute[A]],
  valueEncoder: CirceEncoder[A],
) {
  def toJson: Json = attributeEncoder(attribute)
  def toJsonKV: (String, Json) = (attribute.key.name, valueEncoder(attribute.value))
}
object AttributeWithCirceEncoder {
  given circeEncoder[A]: CirceEncoder[AttributeWithCirceEncoder[A]] = _.toJson

  def fromAttribute[A](attribute: Attribute[A])(using
    attributeEncoder: CirceEncoder[Attribute[A]],
    valueEncoder: CirceEncoder[A],
  ): AttributeWithCirceEncoder[A] =
    apply(attribute, attributeEncoder, valueEncoder)

  given [A]: Conversion[AttributeWithCirceEncoder[A], Attribute[A]] = _.attribute
  given [A](using CirceEncoder[Attribute[A]], CirceEncoder[A]): Conversion[Attribute[A], AttributeWithCirceEncoder[A]] =
    fromAttribute
}
