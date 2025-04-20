package framework.exts

import com.raquo.airstream.ownership.ManualOwner
import com.raquo.airstream.state.Var
import framework.utils.FrameworkTestSuite

import scala.collection.mutable.Buffer
import scala.concurrent.{Future, Promise}

class SignalExtsTests extends FrameworkTestSuite {
  test(".sequentially: events are processed in order") {
    val srcRx = Var(0)
    val promises = Array.fill(3)(Promise[String]())
    val resultSignal = srcRx.signal.sequentially(idx => promises(idx).future)

    val results = Buffer[Option[String]]()
    val owner = new ManualOwner
    resultSignal.foreach(results += _)(owner)

    srcRx.set(1)
    srcRx.set(2)

    for {
      _ <- Future(results.toSeq shouldBe Seq(None))
      _ <- Future(promises(1).success("1"))
      _ <- promises(1).future
      _ <- Future(results.toSeq shouldBe Seq(None))
      _ <- Future(promises(0).success("0"))
      _ <- promises(0).future
      _ <- Future(results.toSeq shouldBe Seq(None, Some("0"), Some("1")))
      _ <- Future(promises(2).success("2"))
      _ <- promises(2).future
      _ <- Future(results.toSeq shouldBe Seq(None, Some("0"), Some("1"), Some("2")))
      _ <- Future(owner.killSubscriptions())
    } yield ()
  }
}
