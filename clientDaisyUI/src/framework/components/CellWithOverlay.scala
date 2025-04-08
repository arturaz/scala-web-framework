package framework.components

import L.*

/** The contents of the overlay for [[CellWithOverlay]]. */
type CellOverlayContents = Signal[Option[Node]]

/** A cell that can have an overlay.
  */
def CellWithOverlay(
  mainCellContents: Modifier[Div]*
)(overlayContents: CellOverlayContents): Div = {
  div(
    cls := "grid",
    div(
      cls := "col-start-1 row-start-1",
      mainCellContents,
    ),
    // Laid out on top to take the same amount of space, so that when the update comes the layout would not change.
    child.maybe <-- overlayContents.splitOption { (_, childSignal) =>
      div(
        cls := "col-start-1 row-start-1",
        child <-- childSignal,
      )
    },
  )
}
