package framework.components

/** Specifies the amount of lines needed for the input. */
enum TextKind derives CanEqual {
  case SingleLine
  case MultiLine
}

/** Typeclass providing the [[TextKind]] for type [[A]].
  *
  * This is usually used in the client side, but because form data types are often shared between the client and server,
  * this typeclass resides in the shared part of the framework.
  */
case class TextKindFor[A](textKind: TextKind) {

  /** Converts the instance to be for another type. Useful when you want to share the implementation between multiple
    * types.
    *
    * Example:
    * {{{
    * given TextKindFor[FieldType] = TextKindFor[FieldFormType].forThisType
    * }}}
    */
  def forThisType[B]: TextKindFor[B] = this.asInstanceOf[TextKindFor[B]]
}
object TextKindFor {
  private val singleLine: TextKindFor[Nothing] = TextKindFor(TextKind.SingleLine)
  private val multiLine: TextKindFor[Nothing] = TextKindFor(TextKind.MultiLine)

  /** Summons the instance. */
  def apply[A](using tkf: TextKindFor[A]): TextKindFor[A] = tkf

  /** Summons the [[TextKind]] for type [[A]]. */
  def kindFor[A](using tkf: TextKindFor[A]): TextKind = tkf.textKind

  def SingleLine[A]: TextKindFor[A] = singleLine.asInstanceOf[TextKindFor[A]]
  def MultiLine[A]: TextKindFor[A] = multiLine.asInstanceOf[TextKindFor[A]]
}
