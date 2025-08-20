package framework.exts

import framework.utils.FrameworkTestSuite

class LongExtsTests extends FrameworkTestSuite {
  test("toBytesPretty") {
    assertEquals(0L.toBytesPretty, "0 b")
    assertEquals(1L.toBytesPretty, "1 b")
    assertEquals(1023L.toBytesPretty, "1.0 kb")
    assertEquals(1024L.toBytesPretty, "1 kb")
    assertEquals(1536L.toBytesPretty, "1.5 kb")
    assertEquals(1048576L.toBytesPretty, "1 mb")
    assertEquals(1073741824L.toBytesPretty, "1 gb")
    assertEquals(1099511627776L.toBytesPretty, "1 tb")
    assertEquals((1024L * 1024 * 1024 * 1024 * 1024).toBytesPretty, "1024 tb")

    // negative values
    assertEquals((-1L).toBytesPretty, "-1 b")
    assertEquals((-1024L).toBytesPretty, "-1 kb")
    assertEquals((-1536L).toBytesPretty, "-1.5 kb")
    assertEquals((-1048576L).toBytesPretty, "-1 mb")
    assertEquals((-1073741824L).toBytesPretty, "-1 gb")
    assertEquals((-1099511627776L).toBytesPretty, "-1 tb")
    assertEquals((-1024L * 1024 * 1024 * 1024 * 1024).toBytesPretty, "-1024 tb")
  }
}
