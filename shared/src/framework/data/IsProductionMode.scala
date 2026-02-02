package framework.data

import framework.utils.NewtypeBoolean

/** Whether we are in production mode. */
object IsProductionMode extends NewtypeBoolean
type IsProductionMode = IsProductionMode.Type
