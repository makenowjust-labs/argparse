package codes.quine.labo.argparse

import minitest.SimpleTestSuite

import Arg._
import FlagName._
import Input._
import Match._
import Failure._
import prelude.Id

object GroupSuite extends SimpleTestSuite {
  test("Group#accept") {
    val flag = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val group = Group(None, Seq(flag))
    assert(group.accept(InputFlag(LongName("foo"))).isDefined)
    assert(group.accept(InputFlag(LongName("bar"))).isEmpty)
  }

  test("Group#accept: ambiguous") {
    val flag = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val group = Group(None, Seq(flag, flag))
    assertEquals(group.accept(InputFlag(LongName("foo"))), Some(Ambiguous()))
  }

  test("Group#validate") {
    val flag = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val group = Group(None, Seq(flag)).validate(_ => Left(Seq("bad")))
    assertEquals(group.accept(InputFlag(LongName("foo"))), Some(Done(Left(Seq(InvalidValue(Some("--foo"), "bad"))))))
  }

  test("Group#<|>") {
    val flag = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val group = Group(None, Seq(flag))
    assertEquals((group <|> group).args.size, 2)
    assertEquals((group <|> group).name, None)
    assertEquals((Group(Some("foo"), Seq(flag)) <|> group).name, Some("foo"))
  }

  test("Group#map") {
    val flag = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val group = Group(None, Seq(flag)).map(x => !x)
    assertEquals(group.accept(InputFlag(LongName("foo"))), Some(Done(Right(Id(false)))))
  }

  test("Group#simpleNames") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(group.simpleNames, Seq("--foo", "--bar"))
  }

  test("Group#usages") {
    val flag1 = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val flag2 = SwitchFlag(Seq(LongName("bar")), "bar", () => Right(true))
    val group = Group(None, Seq(flag1, flag2))
    assertEquals(group.usages, Seq("--foo", "--bar"))
  }
}
