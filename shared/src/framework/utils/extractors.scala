package framework.utils

object ByteE {
  def unapply(s: String): Option[Byte] = Option(s).flatMap(_.toByteOption)
}

object ShortE {
  def unapply(s: String): Option[Short] = Option(s).flatMap(_.toShortOption)
}

object IntE {
  def unapply(s: String): Option[Int] = Option(s).flatMap(_.toIntOption)
}

object LongE {
  def unapply(s: String): Option[Long] = Option(s).flatMap(_.toLongOption)
}

object FloatE {
  def unapply(s: String): Option[Float] = Option(s).flatMap(_.toFloatOption)
}

object DoubleE {
  def unapply(s: String): Option[Double] = Option(s).flatMap(_.toDoubleOption)
}

object BooleanE {
  def unapply(s: String): Option[Boolean] = Option(s).flatMap(_.toBooleanOption)
}
