package framework.localization

import cats.syntax.functor.*
import cats.{Functor, Show}

/** Mix this into a package object to gain localization support for your selected [[LocaleEnum]].
  *
  * Example:
  * {{{
  * package object localization extends framework.localization.LocalizationSupport {
  *   // This line is necessary for a lot of the extension methods to work.
  *   given this.type = this
  *
  *   type LocaleEnum = AppLocale
  * }
  * }}}
  */
trait LocalizationSupport {

  /** The enum that specifies your supported languages. */
  type LocaleEnum

  /** A simple localized value not associated to a type.
    *
    * Example:
    * {{{
    * val Issuer = LocalizedText { case AppLocale.En => "Issuer" }
    * }}}
    */
  trait LocalizedText { self =>

    /** The localized text. */
    def text(using LocaleEnum): String

    /** The localized text, lowercased. */
    def textLO(using locale: LocaleEnum): String = text.toLowerCase()

    /** The localized text, uppercased. */
    def textUP(using locale: LocaleEnum): String = text.toUpperCase()

    /** Applies the [[LocaleEnum]] to this instance. */
    def applied(using locale: LocaleEnum): LocalizationSupport.AppliedLocalizedText = new {
      override def text: String = self.text
    }
  }
  object LocalizedText {

    /** Creates a new instance. */
    def apply(localize: LocaleEnum => String): LocalizedText = new LocalizedText {
      override def text(using locale: LocaleEnum): String = localize(locale)
    }

    /** Creates a new instance that always returns the same string. */
    def always(str: String) = apply(_ => str)

    given (using LocaleEnum): Conversion[LocalizedText, String] = _.text
    given (using LocaleEnum): Show[LocalizedText] = _.text
  }

  /** A localized text with no arguments for the type [[A]]. */
  trait LocalizedTextOf[A] extends LocalizedText {

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
    def of[A](localize: LocaleEnum => String): LocalizedTextOf[A] =
      new {
        override def text(using locale: LocaleEnum): String = localize(locale)
      }

    /** Converts a [[LocalizedText]] to a [[LocalizedTextOf]].
      *
      * Example:
      * {{{
      * given LocalizedTextOf[OrganizationName] = Localization.Name
      * }}}
      */
    given fromLocalizedText[A]: Conversion[LocalizedText, LocalizedTextOf[A]] =
      localizedText =>
        new {
          override def text(using LocaleEnum): String = localizedText.text
        }

    given ltoOfOption[A](using lto: LocalizedTextOf[A]): LocalizedTextOf[Option[A]] =
      of(locale => lto.text(using locale))

    /** Returns the [[LocalizedTextOf]] for the type of the value. */
    inline def forValue[A](value: A)(using lto: LocalizedTextOf[A]): LocalizedTextOf[A] = lto
  }

  extension [F[_], A](fa: F[A]) {
    def mapWithLocalizedText[B](f: A => B)(using
      lto: LocalizedTextOf[A]
    )(using Functor[F], LocaleEnum): F[(B, String)] =
      fa.map { a =>
        val b = f(a)
        (b, lto.text)
      }

    def mapWithLocalizedText[B](
      f: (A, String) => B
    )(using lto: LocalizedTextOf[A])(using Functor[F], LocaleEnum): F[(B, String)] =
      fa.map { a =>
        val text = lto.text
        val b = f(a, text)
        (b, text)
      }
  }

}
object LocalizationSupport {

  /** A [[LocalizationSupport.LocalizedText]] with [[LocalizationSupport.LocaleEnum]] already applied. */
  trait AppliedLocalizedText {

    /** The localized text. */
    def text: String

    /** The localized text, lowercased. */
    def textLO: String = text.toLowerCase()

    /** The localized text, uppercased. */
    def textUP: String = text.toUpperCase()
  }
}
