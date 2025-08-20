package framework.exts

extension (value: Long) {

  /** Returns number of bytes in pretty form, for example 14b, 15.3kb, 16.5mb, 17.8gb, 3.2tb. */
  def toBytesPretty: String = {
    val units = List("b", "kb", "mb", "gb", "tb")
    val maxUnit = units.size - 1
    val unitIdx = math.max(0, math.min(maxUnit, math.log10(math.abs(value).toDouble) / 3).toInt)
    val unit = units(unitIdx)
    val valueInUnit = value / math.pow(1024, unitIdx)

    if (valueInUnit % 1 == 0) s"${valueInUnit.toInt} $unit"
    else f"${valueInUnit}%.1f $unit"
  }
}
