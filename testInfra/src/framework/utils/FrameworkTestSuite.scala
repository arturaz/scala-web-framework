package framework.utils

import munit.CatsEffectSuite

trait FrameworkTestSuite extends CatsEffectSuite with FrameworkTestSuiteHelpers with FrameworkTestSuiteFunFixtureT {
  export cats.syntax.all.*

  val TestValues = new TestValues
}
