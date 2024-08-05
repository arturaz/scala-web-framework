package framework.exts

import alleycats.Extract

/** Needed due to https://github.com/typelevel/cats/issues/4642 */
extension [F[_], A](fa: F[A]) {
  def extract(using X: Extract[F]): A = X.extract(fa)
}
