package codes.quine.labo.argparse

import minitest.SimpleTestSuite

import Arg._
import FlagName._
import Input._
import Failure._

object ArgSpec extends SimpleTestSuite {
  test("Arg#simpleName") {
    val optionFlag = OptionFlag(Seq(LongName("foo")), "<value>", "foo", Right(_))
    val switchFlag = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val positional = Positional("<value>", "foo", Right(_))
    val subcommand = Subcommand(Seq("foo"), "foo", ArgSet.pure(42), Right(_: Int))
    assertEquals(optionFlag.simpleName, "--foo")
    assertEquals(switchFlag.simpleName, "--foo")
    assertEquals(positional.simpleName, "<value>")
    assertEquals(subcommand.simpleName, "foo")
  }

  test("Arg#usage") {
    val optionFlag = OptionFlag(Seq(LongName("foo")), "<value>", "foo", Right(_))
    val switchFlag = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
    val positional = Positional("<value>", "foo", Right(_))
    val subcommand = Subcommand(Seq("foo"), "foo", ArgSet.pure(42), Right(_: Int))
    assertEquals(optionFlag.usage, "--foo=<value>")
    assertEquals(switchFlag.usage, "--foo")
    assertEquals(positional.usage, "<value>")
    assertEquals(subcommand.usage, "foo")
  }

  test("Arg#info") {
    val optionFlag = OptionFlag(Seq(LongName("foo"), ShortName('f')), "<value>", "foo", Right(_))
    val switchFlag = SwitchFlag(Seq(LongName("foo"), ShortName('f')), "foo", () => Right(true))
    val positional = Positional("<value>", "foo", Right(_))
    val subcommand = Subcommand(Seq("foo"), "foo", ArgSet.pure(42), Right(_: Int))
    assertEquals(optionFlag.info, ("--foo=<value>, -f<value>", "foo"))
    assertEquals(switchFlag.info, ("--foo, -f", "foo"))
    assertEquals(positional.info, ("<value>", "foo"))
    assertEquals(subcommand.info, ("foo", "foo"))
  }

  test("OptionFlag#validate") {
    val flag = OptionFlag(Seq(LongName("foo")), "<value>", "foo", Right(_))
      .validate {
        case "foo" => Right(42)
        case _     => Left(Seq("bad"))
      }
      .asInstanceOf[OptionFlag[Int]]
    assertEquals(flag.read("foo"), Right(42))
    assertEquals(flag.read("bar"), Left(Seq("bad")))
  }

  test("OptionFlag#accept") {
    val flag = OptionFlag(Seq(LongName("foo")), "<value>", "foo", Right(_))
    assert(flag.accept(InputFlag(LongName("foo"))).isDefined)
    assert(flag.accept(InputFlag(LongName("bar"))).isEmpty)
    assert(flag.accept(InputSubcommand("foo")).isEmpty)
    assert(flag.accept(InputPositional("foo")).isEmpty)
  }

  test("SwitchFlag#validate") {
    val flag = SwitchFlag(Seq(LongName("foo")), "foo", () => Right(true))
      .validate(_ => Left(Seq("bad")))
      .asInstanceOf[SwitchFlag[Boolean]]
    assertEquals(flag.read(), Left(Seq("bad")))
  }

  test("SwitchFlag#accept") {
    val flag = SwitchFlag(Seq(LongName("foo")), "<value>", () => Right(true))
    assert(flag.accept(InputFlag(LongName("foo"))).isDefined)
    assert(flag.accept(InputFlag(LongName("bar"))).isEmpty)
    assert(flag.accept(InputSubcommand("foo")).isEmpty)
    assert(flag.accept(InputPositional("foo")).isEmpty)
  }

  test("Positional#validate") {
    val positional = Positional("<value>", "foo", Right(_))
      .validate {
        case "foo" => Right(42)
        case _     => Left(Seq("bad"))
      }
      .asInstanceOf[Positional[Int]]
    assertEquals(positional.read("foo"), Right(42))
    assertEquals(positional.read("bar"), Left(Seq("bad")))
  }

  test("Positional#accept") {
    val positional = Positional("<value>", "foo", Right(_))
    assert(positional.accept(InputFlag(LongName("foo"))).isEmpty)
    assert(positional.accept(InputFlag(LongName("bar"))).isEmpty)
    assert(positional.accept(InputSubcommand("foo")).isEmpty)
    assert(positional.accept(InputPositional("foo")).isDefined)
  }

  test("Subcommand#validate") {
    val subcommand = Subcommand(Seq("foo"), "foo", ArgSet.pure(42), Right(_: Int))
      .validate {
        case 42 => Right(42)
        case _  => Left(Seq("bad"))
      }
      .asInstanceOf[Subcommand[Int, Int]]
    assertEquals(subcommand.reads(42), Right(42))
    assertEquals(subcommand.reads(40), Left(Seq(InvalidValue(None, "bad"))))
  }

  test("Subcommand#accept") {
    val subcommand = Subcommand(Seq("foo"), "foo", ArgSet.pure(42), Right(_: Int))
    assert(subcommand.accept(InputFlag(LongName("foo"))).isEmpty)
    assert(subcommand.accept(InputFlag(LongName("bar"))).isEmpty)
    assert(subcommand.accept(InputSubcommand("foo")).isDefined)
    assert(subcommand.accept(InputPositional("foo")).isEmpty)
  }
}
