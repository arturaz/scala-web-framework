package framework.utils

import framework.prelude.CanEqual1
import hedgehog.core.*
import hedgehog.munit.HedgehogAssertions
import hedgehog.{runner as hr, Property}
import munit.{Assertions, FunSuite, Location}

trait HedgehogHelpers extends HedgehogAssertions { self: Assertions =>
  given CanEqual1[Status] = CanEqual.derived

  private val seedSource = hr.SeedSource.fromEnvOrTime()

  private val seed: Seed = Seed.fromLong(seedSource.seed)

  /** Runs a hedgehog property-based test.
    *
    * @see
    *   hedgehog.runner.property
    * @param name
    * @param withConfig
    *   A function with which to change the test PropertyConfig
    * @param prop
    *   The property under test
    * @param loc
    *   The location in the test suite source file
    */
  def check(
    name: String,
    withConfig: PropertyConfig => PropertyConfig = identity,
  )(
    prop: => Property
  )(implicit loc: Location): Unit = {
    val t = hedgehog.runner.property(name, prop).config(withConfig)
    val _ = check(t, t.withConfig(PropertyConfig.default))
  }

  private def check(test: hr.Test, config: PropertyConfig)(implicit
    loc: Location
  ): Any = {
    val report = Property.check(test.withConfig(config), test.result, seed)
    if (report.status != Status.ok) {
      val reason = hr.Test.renderReport(
        this.getClass.getName,
        test,
        report,
        ansiCodesSupported = true,
      )
      val _ = withMunitAssertions(assertions => assertions.fail(s"$reason\n${seedSource.renderLog}"))
    }
  }
}

trait HedgehogSuite extends HedgehogHelpers { self: FunSuite =>

  /** Runs a hedgehog property-based test.
    *
    * @see
    *   hedgehog.runner.property
    * @param name
    * @param withConfig
    *   A function with which to change the test PropertyConfig
    * @param prop
    *   The property under test
    * @param loc
    *   The location in the test suite source file
    */
  def property(
    name: String,
    withConfig: PropertyConfig => PropertyConfig = identity,
  )(
    prop: => Property
  )(implicit loc: Location): Unit = {
    test(name)(check(name, withConfig)(prop))
  }
}
