package framework.exts

import io.scalaland.chimney.PartialTransformer
import io.scalaland.chimney.Transformer

implicit class TransformerExts[From, To](t: Transformer[From, To]) extends AnyVal {
  def andThen[To2](t2: PartialTransformer[To, To2]): PartialTransformer[From, To2] =
    PartialTransformer { from =>
      t2.transform(t.transform(from))
    }

  def andThen[To2](t2: Transformer[To, To2]): Transformer[From, To2] =
    from => t2.transform(t.transform(from))
}
