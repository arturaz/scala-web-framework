package framework.components

import framework.utils.ModificationRequestTracker

/** Helper that determines which CSS classes we should use for buttons which are used for sending or cancelling
  * requests.
  *
  * @param whenCanSend
  *   the classes to use when we can send
  * @param whenCanCancel
  *   the classes to use when we can cancel
  * @param otherwise
  *   the classes to use otherwise
  */
case class SendCancelButtonCssClasses(
  whenCanSend: Seq[String],
  whenCanCancel: Seq[String],
  otherwise: Seq[String],
) {

  /** Applies a function to all sequences. */
  def mapAll(f: Seq[String] => Seq[String]): SendCancelButtonCssClasses =
    copy(whenCanSend = f(whenCanSend), whenCanCancel = f(whenCanCancel), otherwise = f(otherwise))

  def cssClasses(canSendSignal: Signal[Boolean], tracker: ModificationRequestTracker) =
    canSendSignal.combineWithFn(tracker.canCancelBool) {
      case (_, true)      => whenCanCancel
      case (true, false)  => whenCanSend
      case (false, false) => otherwise
    }
}
object SendCancelButtonCssClasses {

  /**   - The button should be in primary color by default when we can send.
    *   - The button should be in accent color by default when we can cancel.
    *   - The button should be of default color by default otherwise.
    */
  val default: SendCancelButtonCssClasses = apply(
    whenCanSend = Seq("btn-primary"),
    whenCanCancel = Seq("btn-accent"),
    otherwise = Seq.empty,
  )

  /**   - The button should be in secondary color by default when we can send.
    *   - The button should be in accent color by default when we can cancel.
    *   - The button should be of default color by default otherwise.
    */
  val secondary: SendCancelButtonCssClasses = apply(
    whenCanSend = Seq("btn-secondary"),
    whenCanCancel = Seq("btn-accent"),
    otherwise = Seq.empty,
  )

  val dangerous: SendCancelButtonCssClasses = apply(
    whenCanSend = Seq("btn-warning"),
    whenCanCancel = Seq("btn-accent"),
    otherwise = Seq.empty,
  )
}
