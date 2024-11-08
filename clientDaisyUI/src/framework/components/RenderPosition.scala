package framework.components

enum RenderPosition derives CanEqual {

  /** The component is rendered to the bottom of other component. */
  case Below

  /** The component is rendered to the side of other component. */
  case Sideways

  def isBelow: Boolean = this match {
    case Below    => true
    case Sideways => false
  }

  def isSideways: Boolean = this match {
    case Below    => false
    case Sideways => true
  }
}
