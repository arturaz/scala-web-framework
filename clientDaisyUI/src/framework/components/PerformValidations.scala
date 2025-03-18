package framework.components

import framework.utils.NewtypeBoolean

/** Whether or not to perform validations on the inputs. */
type PerformValidations = PerformValidations.Type
object PerformValidations extends NewtypeBoolean {

  /** Create a value if performing validations, otherwise returns [[None]]. */
  def whenEnabled[A](value: => A)(using performValidations: PerformValidations): Option[A] =
    if (performValidations) Some(value) else None

  /** Summons a value if performing validations, otherwise returns [[None]]. */
  def whenEnabledSummonValue[A](using PerformValidations)(using value: => A): Option[A] = whenEnabled[A](value)

  /** Summons a value if performing validations, passing it through `transformer`, otherwise returns [[None]]. */
  def summonTransformed[A](transformer: A => A)(using PerformValidations)(using value: => A): Option[A] =
    whenEnabled[A](transformer(value))
}
