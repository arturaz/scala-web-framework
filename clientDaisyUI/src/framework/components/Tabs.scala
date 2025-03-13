package framework.components

import L.*

case class TabsResult[A](rxVar: Var[A], html: Div)
object TabsResult {
  given [A]: Conversion[TabsResult[A], Div] = _.html
}

def Tabs[A: CanEqual1](
  startingTab: A,
  options: NonEmptyVector[A],
  divModifier: Modifier[Div] = emptyMod,
)(tabContents: A => Modifier[Anchor]): TabsResult[A] = {
  val activeTab = Var(startingTab)

  val html = div(
    role := "tablist",
    cls := "tabs",
    divModifier,
    options.toVector.map { option =>
      a(
        role := "tab",
        cls := "tab",
        cls("tab-active") <-- activeTab.signal.map(_ == option),
        onClick --> { _ => activeTab.set(option) },
        tabContents(option),
      )
    },
  )

  TabsResult(activeTab, html)
}
