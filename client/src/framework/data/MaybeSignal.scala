package framework.data

type MaybeSignal[+A] = A | Signal[A]
