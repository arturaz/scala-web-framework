package framework.exts

import framework.db.*

extension (query: Query0[Unit]) {

  /** Returns whether there is at least one result. */
  def single: ConnectionIO[Boolean] = query.option.map(_.isDefined)
}
