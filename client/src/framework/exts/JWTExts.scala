package framework.exts

import framework.data.{FrameworkDateTime, JWT}
import framework.utils.PrettyPrintDuration

extension (signal: Signal[JWT.Expiring]) {

  /** A signal that emits the token when it's valid, and `None` when it's expired. */
  def toOptionSignal(
    logDebug: String => Unit,
    getNow: () => FrameworkDateTime = FrameworkDateTime.now,
    logPrefix: String = "JWT auth:",
  ): Signal[Option[JWT]] = {
    given PrettyPrintDuration.Strings = PrettyPrintDuration.Strings.EnglishShortNoSpaces

    signal.flatMapSwitch { jwt =>
      jwt.expiresAt match {
        case None =>
          log.debug(s"$logPrefix token never expires.")
          Signal.fromValue(Some(jwt.unsafe))

        case Some(expiresAt) =>
          val now = getNow()
          if (now < expiresAt) {
            val untilExpiration = expiresAt - now
            inline def debugStr =
              s"$logPrefix token expires at $expiresAt, in ${untilExpiration.prettyUnbounded}, now is $now"
            EventStream.delay(untilExpiration.toMillis.toInt).mapToSignal {
              case None =>
                log.debug(s"$debugStr, current state: valid")
                Some(jwt.unsafe) // token is still valid
              case Some(()) =>
                log.debug(s"$debugStr, current state: expired")
                None // token expired
            }
          } else {
            // Token has already expired
            log.debug(s"$logPrefix token has already expired at $expiresAt, now is $now")
            Signal.fromValue(None)
          }
      }
    }
  }
}
