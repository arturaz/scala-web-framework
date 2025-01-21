package framework.data

/** Represents a change in the value stream. */
enum ChangedValuesState[+A] {
  case FirstValue(value: A)
  case ChangedValue(previousValue: A, latestValue: A)
}
