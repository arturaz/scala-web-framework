package framework.prelude

import com.raquo.laminar.modifiers.RenderableText

given newtypeRenderableText[TUnderlying, TWrapped](using
  rt: RenderableText[TUnderlying],
  newtype: yantl.Newtype.WithType[TUnderlying, TWrapped],
): RenderableText[TWrapped] with {
  override def asString(value: TWrapped): String = rt.asString(newtype.unwrap(value))
}
