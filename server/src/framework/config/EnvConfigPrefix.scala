package framework.config

import cats.syntax.show.*
import scala.annotation.implicitNotFound

/** A prefix for the environment variables that [[https://cir.is Ciris]] configuration library reads.
  *
  * Example:
  * {{{
  * given cirisConfig[F[_]](using prefix: EnvConfigPrefix): ConfigValue[F, PostgresqlConfig] = (
  *   ciris.env(prefix("POSTGRESQL_USER")).as[String].default("postgres"),
  *   ciris.env(prefix("POSTGRESQL_PASSWORD")).as[String].secret,
  *   ciris.env(prefix("POSTGRESQL_DATABASE")).as[String],
  *   ciris.env(prefix("POSTGRESQL_HOST")).as[Host].default(host"localhost"),
  *   ciris.env(prefix("POSTGRESQL_PORT")).as[Port].default(port"5432"),
  * ).parMapN(apply)
  * }}}
  */
@implicitNotFound(
  "Cannot find an implicit instance of `EnvConfigPrefix`, you probably want to either use `EnvConfigPrefix.app` " +
    "or `EnvConfigPrefix.apply` in scope."
)
final case class EnvConfigPrefix(prefix: String) {
  def apply(key: String): String = show"$prefix$key"
}
object EnvConfigPrefix {

  /** The default value to use, which prefixes with `APP_`. */
  val app: EnvConfigPrefix = apply("APP_")
}
