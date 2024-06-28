package framework.exts

extension [A1, A2](t: (A1, A2)) {
  def map1[B1](f: A1 => B1): (B1, A2) = (f(t._1), t._2)

  def map2[B2](f: A2 => B2): (A1, B2) = (t._1, f(t._2))
}
