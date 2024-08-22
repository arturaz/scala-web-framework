package framework.exts

import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.Transformer

implicit class PartialTransformerExts[From, To](t: PartialTransformer[From, To]) extends AnyVal {
  def andThen[To2](t2: PartialTransformer[To, To2]): PartialTransformer[From, To2] =
    PartialTransformer { from =>
      t.transform(from).flatMap(t2.transform)
    }

  def andThen[To2](t2: Transformer[To, To2]): PartialTransformer[From, To2] =
    PartialTransformer { from =>
      t.transform(from).map(t2.transform)
    }
}
