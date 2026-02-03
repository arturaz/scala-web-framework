package framework.config

import framework.utils.NewtypeBoolean
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attribute
import framework.data.IsProductionMode

/** Are we running in production-like environment? If false, then we are running in development-like environment and
  * data persistence is not important, thus data can be deleted (for example if DB schema is changed in an incompatible
  * way).
  */
object IsProductionModeConfig extends NewtypeBoolean {
  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, IsProductionModeConfig] =
    ciris.env(prefix("IS_PRODUCTION")).as[Boolean].map(apply)

  private val otelAttribute = AttributeKey.boolean("is-production")

  extension (value: Type) {
    def toOtelAttribute: Attribute[Boolean] = otelAttribute(value.unwrap)
  }

  given Conversion[Type, IsProductionMode] = v => IsProductionMode(v.unwrap)
}
type IsProductionModeConfig = IsProductionModeConfig.Type
