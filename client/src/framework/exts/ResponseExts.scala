package framework.exts

import sttp.client3.Response

extension [A](response: Response[A]) {

  /** Transforms the body of the response. */
  def mapBody[B](f: A => B): Response[B] = response.copy(body = f(response.body))
}
