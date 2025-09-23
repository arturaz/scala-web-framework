package framework.exts

import framework.data.FrameworkDate
import concurrent.duration.*

private lazy val _nowUTCSignal: Signal[FrameworkDate] =
  Signal.fromPolling(1.second)(FrameworkDate.nowUTC()).distinct

private lazy val _nowClientSignal: Signal[FrameworkDate] =
  Signal.fromPolling(1.second)(FrameworkDate.nowClient()).distinct

extension (obj: FrameworkDate.type) {

  /** Returns a Signal that contains the current UTC date. */
  def nowUTCSignal: Signal[FrameworkDate] = _nowUTCSignal

  /** Returns a Signal that contains the current client date. */
  def nowClientSignal: Signal[FrameworkDate] = _nowClientSignal
}
