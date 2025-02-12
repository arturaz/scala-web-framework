package framework.prelude

import difflicious.utils.TypeName
import difflicious.{DiffResult, DiffResultPrinter, PairType}

given [A]: CirceCodec[TypeName[A]] = CirceCodec.derived
given CirceCodec[PairType] = CirceCodec.derived
given CirceCodec[DiffResult.MapResult.Entry] = CirceCodec.derived
given CirceCodec[DiffResult] = CirceCodec.derived
