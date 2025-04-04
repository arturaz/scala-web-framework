package framework.components

import L.*

/** A cell that can have an overlay.
  */
def CellWithOverlay(mainCellContents: Modifier[Div]*)(overlayContents: Signal[Option[Modifier[Div]]]) = {
  div(
    cls := "grid",
    div(
      cls := "col-start-1 row-start-1",
      mainCellContents,
    ),
    // Laid out on top to take the same amount of space, so that when the update comes the layout would not change.
    child.maybe <-- overlayContents.mapSome { mods =>
      div(
        cls := "col-start-1 row-start-1",
        mods,
      )
    },
  )
}
