package framework.exts

extension [I, O](f: I => O) {

  inline def tupled: Tuple1[I] => O = i => f(i._1)
}
