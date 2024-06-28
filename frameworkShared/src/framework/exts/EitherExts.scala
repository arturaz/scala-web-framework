package framework.exts

extension [A](self: Either[Nothing, A]) {

  /** Safely gets the value out of [[Either]] as the [[Left]] side can not be instantiated. */
  def getSafe: A = self match {
    case Left(_)      => throw new Exception("impossible")
    case Right(value) => value
  }
}

extension [A, B](e: Either[A, B]) {
  inline def getOrThrow: B = e match {
    case Left(value)  => throw new IllegalStateException(s"Expected Right, got Left($value)")
    case Right(value) => value
  }
}
