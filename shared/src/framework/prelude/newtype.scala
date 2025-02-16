package framework.prelude

import yantl.AsString

given AsString[Any] = AsString.fromToString
