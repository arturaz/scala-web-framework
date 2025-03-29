package framework.prelude

import com.monovore.decline.Argument
import jkugiya.ulid.ULID
import cats.syntax.all.*

given argumentULID: Argument[ULID] = Argument.from("ulid")(ULID.fromString(_).toValidatedNel)
