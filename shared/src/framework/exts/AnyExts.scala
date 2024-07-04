package framework.exts

extension [A](a: A) {
  def pprint: String = _root_.pprint.apply(a).render
}
