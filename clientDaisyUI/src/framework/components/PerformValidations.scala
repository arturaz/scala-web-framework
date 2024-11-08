package framework.components

import framework.utils.NewtypeBoolean

/** Whether or not to perform validations on the inputs. */
type PerformValidations = PerformValidations.Type
object PerformValidations extends NewtypeBoolean {

  /** Create a value if performing validations, otherwise returns [[None]]. */
  def apply[A](value: => A)(using performValidations: PerformValidations): Option[A] =
    if (performValidations) Some(value) else None

  /** Summons a value if performing validations, otherwise returns [[None]]. */
  def summon[A](using PerformValidations)(using value: => A): Option[A] = apply[A](value)
}
