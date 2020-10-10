package codes.quine.labo.argparse

import minitest.SimpleTestSuite

import Arg._
import Failure._
import FlagName._
import Input._
import Match._
import Quantifier._

object QuantifierSuite extends SimpleTestSuite {
  test("Required#accept") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(Required(group).accept(InputFlag(LongName("foo"))), Done(Right(Required(group, Some(true)))))
    assertEquals(Required(group).accept(InputFlag(LongName("baz"))), NoMatch(Required(group)))
  }

  test("Required#result") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(Required(group).result, Left(Seq(MissingArgument(Seq("--foo", "--bar")))))
    assertEquals(Required(group, Some(true)).result, Right(true))
  }

  test("Required#usage") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(Required(Group(None, Seq(flag1))).usage, "--foo")
    assertEquals(Required(Group(None, Seq(flag1, flag2))).usage, "(--foo | --bar)")
  }

  test("Required#args") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(Required(Group(None, Seq(flag1, flag2))).args, Seq(flag1, flag2))
  }

  test("Optional#accept") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(Optional(group).accept(InputFlag(LongName("foo"))), Done(Right(Optional(group, Some(true)))))
    assertEquals(Optional(group).accept(InputFlag(LongName("baz"))), NoMatch(Optional(group)))
  }

  test("Optional#result") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(Optional(group).result, Right(None))
    assertEquals(Optional(group, Some(true)).result, Right(Some(true)))
  }

  test("Optional#usage") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(Optional(Group(None, Seq(flag1))).usage, "[--foo]")
    assertEquals(Optional(Group(None, Seq(flag1, flag2))).usage, "[--foo | --bar]")
  }

  test("Optional#args") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(Optional(Group(None, Seq(flag1, flag2))).args, Seq(flag1, flag2))
  }

  test("RequiredMany#accept") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(RequiredMany(group).accept(InputFlag(LongName("foo"))), Done(Right(RequiredMany(group, Seq(true)))))
    assertEquals(RequiredMany(group).accept(InputFlag(LongName("baz"))), NoMatch(RequiredMany(group)))
  }

  test("RequiredMany#result") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(RequiredMany(group).result, Left(Seq(MissingArgument(Seq("--foo", "--bar")))))
    assertEquals(RequiredMany(group, Seq(true)).result, Right(Seq(true)))
  }

  test("RequiredMany#usage") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(RequiredMany(Group(None, Seq(flag1))).usage, "--foo...")
    assertEquals(RequiredMany(Group(None, Seq(flag1, flag2))).usage, "(--foo | --bar)...")
  }

  test("RequiredMany#args") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(RequiredMany(Group(None, Seq(flag1, flag2))).args, Seq(flag1, flag2))
  }

  test("OptionalMany#accept") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(OptionalMany(group).accept(InputFlag(LongName("foo"))), Done(Right(OptionalMany(group, Seq(true)))))
    assertEquals(OptionalMany(group).accept(InputFlag(LongName("baz"))), NoMatch(OptionalMany(group)))
  }

  test("OptionalMany#result") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(OptionalMany(group).result, Right(Seq.empty))
    assertEquals(OptionalMany(group, Seq(true)).result, Right(Seq(true)))
  }

  test("OptionalMany#usage") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(OptionalMany(Group(None, Seq(flag1))).usage, "[--foo]...")
    assertEquals(OptionalMany(Group(None, Seq(flag1, flag2))).usage, "[--foo | --bar]...")
  }

  test("OptionalMany#args") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(OptionalMany(Group(None, Seq(flag1, flag2))).args, Seq(flag1, flag2))
  }

  test("RequiredOnce#accept") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(RequiredOnce(group).accept(InputFlag(LongName("foo"))), Done(Right(Accepted(true))))
    assertEquals(RequiredOnce(group).accept(InputFlag(LongName("baz"))), NoMatch(RequiredOnce(group)))
  }

  test("RequiredOnce#result") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(RequiredOnce(group).result, Left(Seq(MissingArgument(Seq("--foo", "--bar")))))
  }

  test("RequiredOnce#usage") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(RequiredOnce(Group(None, Seq(flag1))).usage, "--foo")
    assertEquals(RequiredOnce(Group(None, Seq(flag1, flag2))).usage, "(--foo | --bar)")
  }

  test("RequiredOnce#args") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(RequiredOnce(Group(None, Seq(flag1, flag2))).args, Seq(flag1, flag2))
  }

  test("OptionalOnce#accept") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(OptionalOnce(group).accept(InputFlag(LongName("foo"))), Done(Right(Accepted(Some(true)))))
    assertEquals(OptionalOnce(group).accept(InputFlag(LongName("baz"))), NoMatch(OptionalOnce(group)))
  }

  test("OptionalOnce#result") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(OptionalOnce(group).result, Right(None))
  }

  test("OptionalOnce#usage") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(OptionalOnce(Group(None, Seq(flag1))).usage, "[--foo]")
    assertEquals(OptionalOnce(Group(None, Seq(flag1, flag2))).usage, "[--foo | --bar]")
  }

  test("OptionalOnce#args") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(OptionalOnce(Group(None, Seq(flag1, flag2))).args, Seq(flag1, flag2))
  }

  test("Accepted#accept") {
    assertEquals(Accepted(true).accept(InputFlag(LongName("baz"))), NoMatch(Accepted(true)))
  }

  test("Accepted#result") {
    assertEquals(Accepted(true).result, Right(true))
  }

  test("Acceoted#usage") {
    intercept[UnsupportedOperationException](Accepted(true).usage)
    ()
  }

  test("Accepted#args") {
    intercept[UnsupportedOperationException](Accepted(true).args)
    ()
  }

  test("Validation#accept") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    val validate: Boolean => Either[Seq[String], Boolean] = Right(_)
    val validation1 = Validation(Required(group), validate)
    val validation2 = Validation(Required(Group(None, Seq(flag1, flag1))), validate)
    assertEquals(
      validation1.accept(InputFlag(LongName("foo"))),
      Done(Right(Validation(Required(group, Some(true)), validate)))
    )
    assertEquals(validation1.accept(InputFlag(LongName("baz"))), NoMatch(validation1))
    assertEquals(validation2.accept(InputFlag(LongName("foo"))), Ambiguous())
  }

  test("Validation#result") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(
      Validation(Required(group), Right(_: Boolean)).result,
      Left(Seq(MissingArgument(Seq("--foo", "--bar"))))
    )
    assertEquals(Validation(Required(group, Some(true)), Right(_: Boolean)).result, Right(true))
  }

  test("Validation#usage") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(Validation(Required(Group(None, Seq(flag1, flag2))), Right(_: Boolean)).usage, "(--foo | --bar)")
  }

  test("Validation#args") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    assertEquals(Validation(Required(Group(None, Seq(flag1, flag2))), Right(_: Boolean)).args, Seq(flag1, flag2))
  }
}
