package framework.localization

/** Mix this into a package object to gain localization support for your selected [[LocaleEnum]].
  */
trait LocalizationSupport[LocaleEnum] {

  /** A localized text with no arguments for the type [[A]]. */
  trait LocalizedTextOf[A] {

    /** The localized text. */
    def text(using LocaleEnum): String

    /** The localized text, lowercased. */
    def textLO(using locale: LocaleEnum): String = text.toLowerCase()

    /** Converts the instance to be for another type. Useful when you want to share the implementation between multiple
      * types.
      *
      * Example:
      * {{{
      * given LocalizedTextOf[FieldType] = LocalizedTextOf[FieldFormType].forThisType
      * }}}
      */
    def forThisType[B]: LocalizedTextOf[B] = this.asInstanceOf[LocalizedTextOf[B]]
  }
  object LocalizedTextOf {

    /** Allows writing this to retrieve the localized text:
      * {{{
      * LocalizedTextOf[Currency].text
      * }}}
      */
    inline def apply[A](using lto: LocalizedTextOf[A]) = lto

    /** Creates a new instance. */
    def of[A](localize: LocaleEnum => String): LocalizedTextOf[A] = {
      new LocalizedTextOf[A] {
        override def text(using locale: LocaleEnum): String = localize(locale)
      }
    }

    /** Allows geting a localized text from a value.
      *
      * {{{
      * LocalizedTextOf.value(postalCode) // "Postal Code"
      * }}}
      */
    def value[A](value: A)(using lto: LocalizedTextOf[A], locale: LocaleEnum): String = lto.text

    given [A](using lto: LocalizedTextOf[A]): LocalizedTextOf[Option[A]] = of(locale => lto.text(using locale))
  }
}
