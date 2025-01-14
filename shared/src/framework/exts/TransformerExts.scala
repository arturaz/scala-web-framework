package framework.exts

import io.scalaland.chimney.{PartialTransformer, Transformer}

implicit class TransformerExts[From, To](t: Transformer[From, To]) extends AnyVal {
  def andThen[To2](t2: PartialTransformer[To, To2]): PartialTransformer[From, To2] =
    PartialTransformer { from =>
      t2.transform(t.transform(from))
    }

  def andThen[To2](t2: Transformer[To, To2]): Transformer[From, To2] =
    from => t2.transform(t.transform(from))

  def map[To2](f: To => To2): Transformer[From, To2] =
    from => f(t.transform(from))

  def contramap[From2](f: From2 => From): Transformer[From2, To] =
    from => t.transform(f(from))

  def asPartialTransformer: PartialTransformer[From, To] =
    PartialTransformer.fromFunction(t.transform)
}
