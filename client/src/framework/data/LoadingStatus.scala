package framework.data

/** Base trait for various loading statuses. */
trait LoadingStatus[+A] {

  /** Returns true if the resource is loading. */
  def isLoading: Boolean

  /** Returns [[Some]] if the resource is loaded. */
  def toLoadedOption: Option[A]
}
