package codes.quine.labo.argparse

import minitest.SimpleTestSuite

object VCSCommandSuite extends SimpleTestSuite {
  import Failure.{InvalidValue, UnknownArgument}

  test("Command#parse: empty args") {
    val result = VCSCommand.command.parse(Seq())
    assertEquals(result, Right(VCSCommand(false, false, Seq.empty, None, None)))
  }

  test("Command#parse: short flags") {
    val result = VCSCommand.command.parse(Seq("-vhcfoo1=bar1", "-c", "foo2=bar2"))
    assertEquals(result, Right(VCSCommand(true, true, Seq("foo1" -> "bar1", "foo2" -> "bar2"), None, None)))
  }

  test("Command#parse: long flags") {
    val result = VCSCommand.command.parse(Seq("--help", "--config", "foo=bar", "--work-tree=foo"))
    assertEquals(result, Right(VCSCommand(false, true, Seq("foo" -> "bar"), Some("foo"), None)))
  }

  test("Command#parse: subcommand") {
    val result = VCSCommand.command.parse(Seq("--config", "foo=bar", "add", "foo", "bar"))
    assertEquals(
      result,
      Right(
        VCSCommand(false, false, Seq("foo" -> "bar"), None, Some(VCSCommand.Subcommand.Add(false, Seq("foo", "bar"))))
      )
    )
  }

  test("Command#parse: unknown short flag") {
    val result = VCSCommand.command.parse(Seq("-hu"))
    assertEquals(result, Left(Seq(UnknownArgument("-u"))))
  }

  test("Command#parse: unknown long flag") {
    val result = VCSCommand.command.parse(Seq("--foo"))
    assertEquals(result, Left(Seq(UnknownArgument("--foo"))))
  }

  test("Command#parse: unknown positional argument") {
    val result = VCSCommand.command.parse(Seq("foo"))
    assertEquals(result, Left(Seq(UnknownArgument("foo"))))
  }

  test("Command#parse: invalid flag value") {
    val result = VCSCommand.command.parse(Seq("-c", "foo"))
    assertEquals(result, Left(Seq(InvalidValue(Some("-c"), "wrong format: foo"))))
  }

  test("Command#toHelp: main command") {
    val help = VCSCommand.command.toHelp
    assertEquals(
      help.toString,
      """vcs - A version control system
        |
        |Usage:
        |    vcs [--version] [--help] [--config=<name>=<value>]... [--work-tree=<path>] [<subcommand>]
        |
        |Options:
        |    --version, -v        Show a verion number
        |    --help, -h           Show this help
        |    --config=<name>=<value>, -c<name>=<value>
        |                         Pass a configuration parameter to command
        |    --work-tree=<path>   Set the path to the working tree
        |
        |Subcommands:
        |    status               Show the working tree status
        |    add                  Add file contents to the index
        |    commit               Record changes to the repository
        |
        |""".stripMargin
    )
  }

  test("Command#toHelp: subcommand") {
    intercept[IllegalArgumentException](VCSCommand.command.toHelp(Seq("foo")))

    val addHelp = VCSCommand.command.toHelp(Seq("add"))
    assertEquals(
      addHelp.toString,
      """vcs add - Add file contents to the index
        |
        |Usage:
        |    vcs add [--all] <path>...
        |
        |Options:
        |    --all, -A            Add changes from all tracked and untracked files
        |
        |Positional Arguments:
        |    <path>               Paths to add
        |
        |""".stripMargin
    )

    val commitHelp = VCSCommand.command.toHelp(Seq("commit"))
    assertEquals(
      commitHelp.toString,
      """vcs commit - Record changes to the repository
        |
        |Usage:
        |    vcs commit [--allow-empty] --message=<text>
        |
        |Options:
        |    --allow-empty        Allow an empty commit
        |    --message=<text>, -m<text>
        |                         Set a commit message
        |
        |""".stripMargin
    )

    val statusHelp = VCSCommand.command.toHelp(Seq("status"))
    assertEquals(
      statusHelp.toString,
      """vcs status - Show the working tree status
        |
        |Usage:
        |    vcs status [--short] [<path>]...
        |
        |Options:
        |    --short, -s          Give the output in the short-format
        |
        |Positional Arguments:
        |    <path>               Paths to show status
        |
        |""".stripMargin
    )
  }
}
