package framework.data

/** Typeclass that indicates the last version number for a data type.
  *
  * Example:
  * {{{
  * given lastVersionFor: LastVersionFor.Aux[GeneralPracticeUserSettings, Version] = LastVersionFor.`for`.is(Version.V0)
  * }}}
  */
trait LastVersionFor[TData] {
  type TVersion

  def last: TVersion
}
object LastVersionFor {
  type Aux[TData, TVersion_] = LastVersionFor[TData] { type TVersion = TVersion_ }

  def `for`[TData]: ForBuilder[TData] = ForBuilder(())

  case class ForBuilder[TData](private val dummy: Unit) extends AnyVal {
    def is[TVersion_](version: TVersion_): LastVersionFor.Aux[TData, TVersion_] =
      new LastVersionFor[TData] {
        type TVersion = TVersion_

        def last: TVersion = version
      }
  }
}
