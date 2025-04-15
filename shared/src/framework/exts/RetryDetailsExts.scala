package framework.exts

import retry.RetryDetails
import framework.prelude.given
import framework.utils.PrettyPrintDuration

extension (nextStep: RetryDetails.NextStep) {
  def asString: String = nextStep match {
    case RetryDetails.NextStep.GiveUp => "GiveUp"
    case RetryDetails.NextStep.DelayAndRetry(nextDelay) =>
      show"DelayAndRetry(nextDelay=${nextDelay.prettyForDebug(using PrettyPrintDuration.Strings.EnglishShortNoSpaces)})"
  }
}

extension (details: RetryDetails) {
  def asString: String = {
    val RetryDetails(retriesSoFar, cumulativeDelay, nextStepIfUnsuccessful) = details

    show"RetryDetails(retriesSoFar=$retriesSoFar, " +
      show"cumulativeDelay=${cumulativeDelay.prettyForDebug(using PrettyPrintDuration.Strings.EnglishShortNoSpaces)}, " +
      show"nextStepIfUnsuccessful=${nextStepIfUnsuccessful.asString})"
  }
}
