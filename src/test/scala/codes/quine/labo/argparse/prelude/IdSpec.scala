package codes.quine.labo.argparse
package prelude

import minitest.SimpleTestSuite

object IdSpec extends SimpleTestSuite {
  test("Id#value") {
    assertEquals(Id(42).value, 42)
  }
}
