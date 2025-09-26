package framework.exts

import difflicious.Differ
import difflicious.DiffInput

extension (obj: Differ.type) {

  /** Summons a given [[Differ]] instance and computes the diff between two values. */
  def diff[A](obtained: A, expected: A)(using differ: Differ[A]): differ.R =
    differ.diff(obtained, expected)

  def diff[A](inputs: DiffInput[A])(using differ: Differ[A]): differ.R =
    differ.diff(inputs)
}
