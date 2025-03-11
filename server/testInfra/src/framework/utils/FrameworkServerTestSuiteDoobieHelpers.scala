package framework.utils

import cats.effect.IO
import cats.effect.std.Dispatcher
import doobie.munit.analysisspec.Checker.ErrorItems
import doobie.util.Colors
import doobie.util.testing.*
import framework.db.*
import hedgehog.Gen
import hedgehog.core.{Log, PropertyConfig, Result}
import munit.{Assertions, CatsEffectSuite, Location}

trait FrameworkServerTestSuiteDoobieHelpers extends GenDerivation with HedgehogHelpers {
  self: CatsEffectSuite & Assertions =>

  def doobieCheckerColors: Colors = Colors.Ansi

  /** Checks an SQL query using hedgehog generators to create that query.
    *
    * Example:
    * {{{
    * myTransactorFixture.testGiven("GeneralPractices.queryLocation")(checkSql()(GeneralPractices.queryLocation.tupled))
    * }}}
    */
  def checkSql[T <: Tuple, A: Analyzable](withConfig: PropertyConfig => PropertyConfig = identity)(
    make: T => A
  )(using loc: Location, gen: Gen[T], xa: Transactor[IO]): IO[Unit] =
    Dispatcher.parallel[IO](await = true).use { dispatcher =>
      IO {
        check("SQL validity", withConfig)(gen.forAll.map { input =>
          val query = make(input)
          val result = dispatcher.unsafeRunSync(checkSqlResult(query))
          // println(s"$input -> $query: ${result.args} -> ${result.report}")
          result.asHedgehogResult
        })
      }
    }

  /** Checks an SQL query failing the test if the check does not succeed.
    *
    * Example:
    * {{{
    * myTransactorFixture.testGiven("GeneralPractices.queryLocation") {
    *   checkSql()(GeneralPractices.queryLocation(myParameter1, myParameter2)
    * }
    * }}}
    */
  def checkSql[A: Analyzable](a: A)(using xa: Transactor[IO], loc: Location): IO[Unit] =
    checkSqlResult(a).flatMap(result =>
      if (result.report.succeeded) IO.unit else IO(fail(message = result.message, cause = result.exception))
    )

  /** Checks an SQL query producing a result. */
  def checkSqlResult[A: Analyzable](a: A)(using xa: Transactor[IO], loc: Location): IO[CheckSqlResult] = {
    val args = Analyzable.unpack(a)

    analyze(args).transact(xa).map(report => CheckSqlResult(args, report))
  }

  case class CheckSqlResult(args: AnalysisArgs, report: AnalysisReport) {
    def message: String = formatReport(args, report, doobieCheckerColors).padLeft("  ").toString

    def exception: ErrorItems = ErrorItems(report.items.filter(_.error.isDefined))

    def asHedgehogResult: Result =
      if (report.succeeded) Result.Success else Result.Failure(message :: hedgehog.core.Error(exception) :: Nil)
  }
}
