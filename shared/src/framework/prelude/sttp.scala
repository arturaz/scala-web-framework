package framework.prelude

import sttp.model.Uri

given Show[Uri] = _.toString
