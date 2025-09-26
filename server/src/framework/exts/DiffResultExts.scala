package framework.exts

import difflicious.DiffResult

extension (result: DiffResult) {

  /** Converts from a more specific [[DiffResult]] to a generic one. */
  def widen: DiffResult = result
}
