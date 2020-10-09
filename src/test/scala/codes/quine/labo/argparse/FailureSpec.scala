package codes.quine.labo.argparse

import minitest.SimpleTestSuite

import Failure._

object FailureSpec extends SimpleTestSuite {
  test("AmbiguousArgument#errorMessage") {
    assertEquals(AmbiguousArgument("foo").errorMessage, "ambiguous argument: foo")
  }

  test("UnknownArgument#errorMessage") {
    assertEquals(UnknownArgument("foo").errorMessage, "unknown argument: foo")
  }

  test("MissingValue#errorMessage") {
    assertEquals(MissingValue("foo").errorMessage, "missing value: foo")
  }

  test("InvalidValue#errorMessage") {
    assertEquals(InvalidValue(None, "message").errorMessage, "invalid value: message")
    assertEquals(InvalidValue(Some("foo"), "message").errorMessage, "invalid value: foo: message")
  }

  test("MissingArgument#errorMessage") {
    assertEquals(MissingArgument(Seq()).errorMessage, "missing required argument")
    assertEquals(MissingArgument(Seq("foo")).errorMessage, "missing required argument: foo")
    assertEquals(MissingArgument(Seq("foo", "bar")).errorMessage, "missing required argument: foo or bar")
  }
}
