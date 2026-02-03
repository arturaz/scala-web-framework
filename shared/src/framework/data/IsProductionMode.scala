package framework.data

import framework.utils.NewtypeBoolean

/** Are we running in production-like environment? */
object IsProductionMode extends NewtypeBoolean
type IsProductionMode = IsProductionMode.Type
