package framework.redis

import framework.prelude.given // Show[ULID]
import framework.utils.FrameworkTestSuite
import jkugiya.ulid.ULID

/** Unit tests for the [[RedisChannelLayout]] abstraction: that one layout drives both the concrete channel and
  * its subscription patterns, and that the invalid states it is meant to rule out do not compile.
  */
class RedisChannelLayoutTest extends FrameworkTestSuite {

  private given prefix: RedisKeyPrefix[String] = RedisKeyPrefix("")

  private def ulid(seed: Byte): ULID = ULID.fromBinary(Array.fill[Byte](16)(seed))
  private val a = ulid(1)
  private val b = ulid(2)

  // ULID renders as its base32 form (Show[ULID] = _.base32), which is what the layout uses for `arg[ULID]`.
  private def s(u: ULID): String = u.base32

  test("channel: literals and args render joined by ':'") {
    val layout = RedisChannelLayout.of[String].lit("gp").arg[ULID].lit("foo-updates").arg[ULID]
    layout.channel((a, b)).channel.underlying shouldBe s"gp:${s(a)}:foo-updates:${s(b)}"
  }

  test("channel: the key prefix is prepended exactly once") {
    given prefixed: RedisKeyPrefix[String] = RedisKeyPrefix("rapix:test:")
    val layout = RedisChannelLayout.of[String].lit("gp").arg[ULID]
    layout.channel(Tuple1(a))(using prefixed).channel.underlying shouldBe s"rapix:test:gp:${s(a)}"
  }

  test("pattern: a wildcard slot renders as '*', concrete slots render their value") {
    val layout = RedisChannelLayout.of[String].lit("gp").arg[ULID].lit("foo-updates").arg[ULID]
    layout.pattern((SlotMatch.Wildcard, SlotMatch.Exact(b))).pattern.underlying shouldBe s"gp:*:foo-updates:${s(b)}"
    layout.pattern((SlotMatch.Exact(a), SlotMatch.Wildcard)).pattern.underlying shouldBe s"gp:${s(a)}:foo-updates:*"
    layout.pattern((SlotMatch.Wildcard, SlotMatch.Wildcard)).pattern.underlying shouldBe "gp:*:foo-updates:*"
  }

  test("channel and pattern derive from the same layout, so an all-concrete pattern equals the channel") {
    val layout = RedisChannelLayout.of[String].lit("ns").arg[ULID].lit("mid").arg[ULID]
    layout.pattern((SlotMatch.Exact(a), SlotMatch.Exact(b))).pattern.underlying shouldBe
      layout.channel((a, b)).channel.underlying
  }

  test("single-slot layout") {
    val layout = RedisChannelLayout.of[String].lit("only").arg[ULID]
    layout.channel(Tuple1(a)).channel.underlying shouldBe s"only:${s(a)}"
    layout.pattern(Tuple1(SlotMatch.Wildcard)).pattern.underlying shouldBe "only:*"
  }

  test("zero-slot layout renders just its literals") {
    val layout = RedisChannelLayout.of[String].lit("a").lit("b")
    layout.channel(EmptyTuple).channel.underlying shouldBe "a:b"
  }

  test("a trailing literal after an arg is rendered") {
    val layout = RedisChannelLayout.of[String].lit("drug-req").arg[ULID].lit("updates")
    layout.channel(Tuple1(a)).channel.underlying shouldBe s"drug-req:${s(a)}:updates"
  }

  test("argWith: renders a slot whose type has no Show") {
    final case class Target(kind: String, id: Int)
    val layout = RedisChannelLayout.of[String].lit("comments").argWith[Target](t => s"${t.kind}:${t.id}")
    layout.channel(Tuple1(Target("drug_request", 7))).channel.underlying shouldBe "comments:drug_request:7"
  }

  test("compile-time: a wildcard cannot appear in a concrete channel (channel takes values)") {
    val errs = compileErrors(
      """RedisChannelLayout.of[String].lit("gp").arg[jkugiya.ulid.ULID].channel(Tuple1(SlotMatch.Wildcard))"""
    )
    assert(errs.nonEmpty, errs)
  }

  test("compile-time: slots cannot be reordered (distinct slot types)") {
    // layout slots are (ULID, Int); passing (Int, ULID) must not compile
    val errs = compileErrors(
      """RedisChannelLayout.of[String].lit("a").arg[jkugiya.ulid.ULID].lit("b").argWith[Int](_.toString).channel((1, a))"""
    )
    assert(errs.nonEmpty, errs)
  }
}
