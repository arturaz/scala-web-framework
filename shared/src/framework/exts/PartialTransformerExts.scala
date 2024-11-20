package framework.exts

import io.scalaland.chimney.{partial, PartialTransformer, Transformer}

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

extension (obj: PartialTransformer.type) {
  def fromEitherString[From, To](f: From => Either[String, To]): PartialTransformer[From, To] =
    PartialTransformer(from => partial.Result.fromEitherString(f(from)))
}
