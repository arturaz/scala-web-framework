package framework.utils

import com.raquo.airstream.state.StrictSignal

/** Exposes the current state of the page focus. */
object PageFocus {
  private val rxVar = Var(window.document.hasFocus())
  def signal: StrictSignal[Boolean] = rxVar.signal

  window.addEventListener("focus", _ => rxVar.set(true))
  window.addEventListener("blur", _ => rxVar.set(false))
}
