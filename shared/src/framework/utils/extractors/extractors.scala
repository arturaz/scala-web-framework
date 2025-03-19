package framework.utils.extractors

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

/** Matches a string starting with a given prefix. Returns the rest of the string.
  *
  * Example:
  * ```scala
  * val StartsWithHello = StartsWithE("hello")
  * "hello world" match { case StartsWithHello(rest) => rest } // " world"
  * ```
  */
case class StartsWithE(prefix: String) {
  def unapply(s: String): Option[String] = Option(s).flatMap { s =>
    if (s.startsWith(prefix)) Some(s.substring(prefix.length))
    else None
  }
}

/** Matches a string ending with a given suffix. Returns the prefix of the string.
  *
  * Example:
  * ```scala
  * val EndsWithWorld = EndsWithE("world")
  * "hello world" match { case EndsWithWorld(prefix) => prefix } // "hello "
  * ```
  */
case class EndsWithE(suffix: String) {
  def unapply(s: String): Option[String] = Option(s).flatMap { s =>
    if (s.endsWith(suffix)) Some(s.substring(0, s.length - suffix.length))
    else None
  }
}
