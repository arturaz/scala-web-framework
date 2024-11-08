package framework.components

enum ComponentStyle derives CanEqual {

  /** The component is being rendered standalone. */
  case Standalone

  /** The component is being rendered inside a table cell. */
  case TableCell

  def isTableCell: Boolean = this match {
    case Standalone => false
    case TableCell  => true
  }
}
