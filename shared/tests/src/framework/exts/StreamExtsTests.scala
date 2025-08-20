package framework.exts

import fs2.Stream

import framework.utils.FrameworkTestSuite
import cats.effect.IO
import fs2.Chunk

class StreamExtsTests extends FrameworkTestSuite {
  test("evalMapChunksWithItemCount") {
    val stream = Stream[IO, Int](1, 2, 3) ++ Stream(4, 5) ++ Stream(6)

    val actual = stream
      .evalMapChunksWithItemCount((count, chunk) => IO.pure(Chunk((count, chunk.size))))
      .compile
      .toVector

    val expected = Vector((3L, 3), (5L, 2), (6L, 1))
    assertIO(actual, expected)
  }

}
