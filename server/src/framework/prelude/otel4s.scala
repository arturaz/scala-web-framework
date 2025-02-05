package framework.prelude

import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeType
import io.circe.Json

given [A]: CanEqual1[AttributeType[A]] = CanEqual.derived

given attributeCirceEncoder[A]: CirceEncoder[Attribute[A]] = a => {
  val valueJson = a.key.`type` match {
    case AttributeType.Boolean    => Json.fromBoolean(a.value.asInstanceOf)
    case AttributeType.Double     => Json.fromDoubleOrString(a.value.asInstanceOf)
    case AttributeType.String     => Json.fromString(a.value.asInstanceOf)
    case AttributeType.Long       => Json.fromLong(a.value.asInstanceOf)
    case AttributeType.BooleanSeq => Json.arr(a.value.asInstanceOf[Seq[Boolean]].map(Json.fromBoolean)*)
    case AttributeType.DoubleSeq  => Json.arr(a.value.asInstanceOf[Seq[Double]].map(Json.fromDoubleOrString)*)
    case AttributeType.StringSeq  => Json.arr(a.value.asInstanceOf[Seq[String]].map(Json.fromString)*)
    case AttributeType.LongSeq    => Json.arr(a.value.asInstanceOf[Seq[Long]].map(Json.fromLong)*)
  }

  Json.arr(Json.fromString(a.key.name), valueJson)
}
