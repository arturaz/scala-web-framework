package ciris

import io.circe.Decoder
import scala.reflect.ClassTag
import cats.syntax.show

/** Retrieves a value from environment, reading it as a JSON array. */
def envFromJsonArray[F[_], C[_], A](name: String)(using
  decoder: Decoder[C[A]],
  collectionClassTag: ClassTag[C[A]],
  elementClassTag: ClassTag[A],
): ConfigValue[F, C[A]] = {
  val configKey = ConfigKey.env(name)

  env(name).flatMap { str =>
    io.circe.parser.decode[C[A]](str) match {
      case Left(error) =>
        val typeName = show"${collectionClassTag.runtimeClass.getName()}[${elementClassTag.runtimeClass.getName()}]"
        ConfigValue.failed(ConfigError.decode(typeName = typeName, key = Some(configKey), value = str))

      case Right(value) =>
        ConfigValue.pure(ConfigEntry.loaded(Some(configKey), value))
    }
  }
}
