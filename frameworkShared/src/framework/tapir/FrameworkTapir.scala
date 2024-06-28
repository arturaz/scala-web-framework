package framework.tapir

import sttp.tapir.Tapir
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.integ.cats.{MonadErrorSyntax, ServerEndpointSyntax, TapirCodecCats}
import sttp.tapir.json.circe.TapirJsonCirce

object FrameworkTapir
    extends Tapir
    with SchemaDerivation
    with TapirCodecCats
    with TapirJsonCirce
    with ServerEndpointSyntax
    with MonadErrorSyntax
