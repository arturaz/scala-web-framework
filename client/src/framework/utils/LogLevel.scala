package framework.utils

enum LogLevel derives CanEqual {

  /** Level that you get for [[console.log]]. */
  case Default

  /** Level that you get for [[console.debug]]. */
  case Debug

  /** Level that you get for [[console.info]]. */
  case Info

  /** Level that you get for [[console.warn]]. */
  case Warning

  /** Level that you get for [[console.error]]. */
  case Error
}
