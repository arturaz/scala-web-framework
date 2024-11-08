package framework.exts

import squants.market.{Currency, MoneyContext}

extension (mc: MoneyContext) {

  /** Returns a list of currencies for use with a select HTML tag. */
  def currenciesAsSelectOptions: Vector[(Currency, String)] =
    mc.currencies.iterator.map(c => (c, s"${c.name.show} (${c.code.show})")).toVector
}
