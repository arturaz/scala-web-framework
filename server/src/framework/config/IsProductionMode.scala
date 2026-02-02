package framework.config

import framework.utils.NewtypeBoolean
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attribute

/** Are we running in production-like environment? If false, then we are running in development-like environment and
  * data persistence is not important, thus data can be deleted (for example if DB schema is changed in an incompatible
  * way).
  */
object IsProductionMode extends NewtypeBoolean {
  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, IsProductionMode] =
    ciris.env(prefix("IS_PRODUCTION")).as[Boolean].map(apply)

  private val otelAttribute = AttributeKey.boolean("is-production")

  extension (value: Type) {
    def toOtelAttribute: Attribute[Boolean] = otelAttribute(value.unwrap)
  }

  given Conversion[Type, framework.data.IsProductionMode] = v => framework.data.IsProductionMode(v.unwrap)
}
type IsProductionMode = IsProductionMode.Type
