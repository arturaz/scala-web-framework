package framework.exts

extension [A1, A2, R](f: (A1, A2) => R) {
  def andThen[R1](f1: R => R1): (A1, A2) => R1 = (a1, a2) => f1(f(a1, a2))
}

extension [A1, A2, A3, R](f: (A1, A2, A3) => R) {
  def andThen[R1](f1: R => R1): (A1, A2, A3) => R1 = (a1, a2, a3) => f1(f(a1, a2, a3))
}

extension [A1, A2, A3, A4, R](f: (A1, A2, A3, A4) => R) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4) => R1 = (a1, a2, a3, a4) => f1(f(a1, a2, a3, a4))
}

extension [A1, A2, A3, A4, A5, R](f: (A1, A2, A3, A4, A5) => R) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5) => R1 = (a1, a2, a3, a4, a5) => f1(f(a1, a2, a3, a4, a5))
}

extension [A1, A2, A3, A4, A5, A6, R](f: (A1, A2, A3, A4, A5, A6) => R) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5, A6) => R1 = (a1, a2, a3, a4, a5, a6) =>
    f1(f(a1, a2, a3, a4, a5, a6))
}

extension [A1, A2, A3, A4, A5, A6, A7, R](f: (A1, A2, A3, A4, A5, A6, A7) => R) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5, A6, A7) => R1 = (a1, a2, a3, a4, a5, a6, a7) =>
    f1(f(a1, a2, a3, a4, a5, a6, a7))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, R](f: (A1, A2, A3, A4, A5, A6, A7, A8) => R) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5, A6, A7, A8) => R1 = (a1, a2, a3, a4, a5, a6, a7, a8) =>
    f1(f(a1, a2, a3, a4, a5, a6, a7, a8))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, R](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => R) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5, A6, A7, A8, A9) => R1 = (a1, a2, a3, a4, a5, a6, a7, a8, a9) =>
    f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, R](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => R) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) => f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, R](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => R) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) => f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, R](
  f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => R
) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) => f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, R](
  f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => R
) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) =>
      f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, R](
  f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => R
) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) =>
      f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, R](
  f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => R
) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) =>
      f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, R](
  f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => R
) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) =>
      f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, R](
  f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => R
) {
  def andThen[R1](f1: R => R1): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) =>
      f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, R](
  f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => R
) {
  def andThen[R1](
    f1: R => R1
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) =>
      f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, R](
  f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => R
) {
  def andThen[R1](
    f1: R => R1
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) =>
      f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, R](
  f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => R
) {
  def andThen[R1](
    f1: R => R1
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) =>
      f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, R](
  f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => R
) {
  def andThen[R1](
    f1: R => R1
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) =>
      f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21))
}

extension [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, R](
  f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => R
) {
  def andThen[R1](
    f1: R => R1
  ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => R1 =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) =>
      f1(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22))
}
