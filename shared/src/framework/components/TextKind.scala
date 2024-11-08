package framework.components

/** Specifies the amount of text. */
enum TextKind derives CanEqual {
  case SingleLine
  case MultiLine
}

/** Typeclass providing the [[TextKind]] for type [[A]].
  *
  * This is usually used in the client side, but because form data types are often shared between the client and server,
  * this typeclass resides in the shared part of the framework.
  */
case class TextKindFor[A](textKind: TextKind)
object TextKindFor {
  private val singleLine: TextKindFor[Nothing] = TextKindFor(TextKind.SingleLine)
  private val multiLine: TextKindFor[Nothing] = TextKindFor(TextKind.MultiLine)

  def SingleLine[A]: TextKindFor[A] = singleLine.asInstanceOf[TextKindFor[A]]
  def MultiLine[A]: TextKindFor[A] = multiLine.asInstanceOf[TextKindFor[A]]
}
