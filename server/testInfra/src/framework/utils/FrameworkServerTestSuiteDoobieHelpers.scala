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
import org.tpolecat.typename.*

import java.sql.SQLException

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
  def checkSql[T <: Tuple, A: AnalyzableRunnable](withConfig: PropertyConfig => PropertyConfig = identity)(
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
  def checkSql[A: AnalyzableRunnable](a: A)(using xa: Transactor[IO], loc: Location): IO[Unit] =
    checkSqlResult(a).flatMap(_.asIO)

  /** Checks an SQL query producing a result. */
  def checkSqlResult[A](
    a: A
  )(using xa: Transactor[IO], loc: Location, ar: AnalyzableRunnable[A]): IO[CheckSqlResult] = {
    val args = Analyzable.unpack(a)

    val dbIO = for {
      report <- analyze(args)
      // If this is runnable, try to actually run it.
      maybeExecutionError <- ar
        .run(a)
        .fold(ConnectionIO.pure(None))(query => query.to[List].attemptSql.map(_.left.toOption))
    } yield CheckSqlResult(args, report, maybeExecutionError)

    dbIO.transact(xa)
  }

  case class CheckSqlResult(args: AnalysisArgs, report: AnalysisReport, maybeExecutionError: Option[SQLException]) {
    def message: String = formatReport(args, report, doobieCheckerColors).padLeft("  ").toString

    def exception: ErrorItems = ErrorItems(report.items.filter(_.error.isDefined))

    def asHedgehogResult: Result = {
      (report.succeeded, maybeExecutionError) match {
        case (true, None)  => Result.Success
        case (false, None) => Result.Failure(message :: hedgehog.core.Error(exception) :: Nil)
        case (true, Some(error)) =>
          Result.Failure("typecheck succeeded, but execution failed" :: hedgehog.core.Error(error) :: Nil)
        case (false, Some(error)) =>
          Result.Failure(
            hedgehog.core.Info("both typecheck and execution failed") ::
              hedgehog.core.Info(message) ::
              hedgehog.core.Error(exception) ::
              hedgehog.core.Error(error) :: Nil
          )
      }
    }

    def asIO: IO[Unit] = {
      (report.succeeded, maybeExecutionError) match {
        case (true, None)        => IO.unit
        case (false, None)       => IO(fail(message = message, cause = exception))
        case (true, Some(error)) => IO(fail(message = "typecheck succeeded, but execution failed", cause = error))
        case (false, Some(error)) =>
          IO(
            fail(
              message = s"both typecheck and execution failed. $message",
              cause = CheckSqlResult.AggregateFailure(exception, error),
            )
          )
      }
    }
  }
  object CheckSqlResult {
    case class AggregateFailure(errorItems: ErrorItems, sqlException: SQLException) extends Exception
  }

  /** [[Analyzable]] extended with the ability to actually perform the queries. */
  trait AnalyzableRunnable[T] extends Analyzable[T] {
    // def runWithInput(t: T): Option[Input => Query0[Any]]
    def run(t: T): Option[Query0[Any]]
  }
  object AnalyzableRunnable {
    given analyzableQuery[A: TypeName, B: TypeName]: AnalyzableRunnable[Query[A, B]] with {
      override def unpack(q: Query[A, B]): AnalysisArgs = Analyzable.analyzableQuery.unpack(q)
      // override def runWithInput(t: Query[A, B]): Option[A => Query0[Any]] = Some(t.toQuery0(_).map(v => v: Any))
      override def run(t: Query[A, B]): Option[Query0[Any]] = None
    }

    given analyzableQuery0[A: TypeName]: AnalyzableRunnable[Query0[A]] with {
      override def unpack(q: Query0[A]): AnalysisArgs = Analyzable.analyzableQuery0.unpack(q)
      // override def runWithInput(t: Query0[A]): Option[Unit => Query0[Any]] = None
      override def run(t: Query0[A]): Option[Query0[Any]] = Some(t.map(v => v: Any))
    }

    given analyzableUpdate[A: TypeName]: AnalyzableRunnable[Update[A]] with {
      override def unpack(q: Update[A]): AnalysisArgs = Analyzable.analyzableUpdate.unpack(q)
      // override def runWithInput(t: Update[A]): Option[A => Query0[Any]] = None
      override def run(t: Update[A]): Option[Query0[Any]] = None
    }

    given analyzableUpdate0: AnalyzableRunnable[Update0] with {
      override def unpack(q: Update0): AnalysisArgs = Analyzable.analyzableUpdate0.unpack(q)
      // override def runWithInput(t: Update0) = None
      override def run(t: Update0) = None
    }
  }
}
