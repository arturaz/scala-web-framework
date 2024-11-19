package framework.config

import framework.utils.NewtypeBoolean

/** Are we running in production-like environment? If false, then we are running in development-like environment and
  * data persistence is not important, thus data can be deleted (for example if DB schema is changed in an incompatible
  * way).
  */
object IsProductionMode extends NewtypeBoolean {
  given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, IsProductionMode] =
    ciris.env(prefix("IS_PRODUCTION")).as[Boolean].map(create)
}
type IsProductionMode = IsProductionMode.Type
