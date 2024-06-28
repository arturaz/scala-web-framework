package framework.prelude

import org.http4s.Method
import org.http4s.Uri

given CanEqual[Method, Method] = CanEqual.derived
given CanEqual[Uri.Path, Uri.Path] = CanEqual.derived
