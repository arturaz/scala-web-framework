package framework.utils

import com.raquo.laminar.modifiers.RenderableNode
import com.raquo.laminar.nodes.ChildNode

/** Helper to quickly turn any class into a [[RenderableNode]]. */
trait IsRenderableNode {
  def render: ChildNode.Base
}
object IsRenderableNode {
  given renderableNode[A <: IsRenderableNode]: RenderableNode[A] = RenderableNode(_.render)
}
