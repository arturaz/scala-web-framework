package framework.exts

import org.http4s.{headers, Request}

extension [F[_]](req: Request[F]) {

  /** Tries to extract the host from the request. */
  def extractHost: Either[String, headers.Host] = {
    // HTTP 2
    def hostFromAuthority = req.uri.authority.map(authority => headers.Host(authority.host.show, authority.port))
    def hostFromHeaders = req.headers.get[headers.Host]

    (hostFromAuthority orElse hostFromHeaders).toRight("Either authority or 'Host' header is required.")
  }
}
