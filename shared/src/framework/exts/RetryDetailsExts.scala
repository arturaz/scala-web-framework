package framework.exts

import retry.RetryDetails
import framework.prelude.given
import framework.utils.PrettyPrintDuration

implicit class RetryDetailsNextStepExts(private val nextStep: RetryDetails.NextStep) extends AnyVal {
  def asString: String = nextStep match {
    case RetryDetails.NextStep.GiveUp => "GiveUp"
    case RetryDetails.NextStep.DelayAndRetry(nextDelay) =>
      show"DelayAndRetry(nextDelay=${nextDelay.prettyForDebug(using PrettyPrintDuration.Strings.EnglishShortNoSpaces)})"
  }
}

implicit class RetryDetailsExts(private val details: RetryDetails) extends AnyVal {
  def asString: String = {
    val RetryDetails(retriesSoFar, cumulativeDelay, nextStepIfUnsuccessful) = details

    show"RetryDetails(retriesSoFar=$retriesSoFar, " +
      show"cumulativeDelay=${cumulativeDelay.prettyForDebug(using PrettyPrintDuration.Strings.EnglishShortNoSpaces)}, " +
      show"nextStepIfUnsuccessful=${nextStepIfUnsuccessful.asString})"
  }
}
