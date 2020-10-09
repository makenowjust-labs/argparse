package codes.quine.labo.argparse

import minitest.SimpleTestSuite

object CommandSpec extends SimpleTestSuite {
  test("Command#toHelp: main command") {
    val help = VCSCommand.command.toHelp
    assertEquals(
      help.toString,
      """vcs - A version control system
        |
        |Usage:
        |    vcs [--version] [--help] [--config=<name>=<value>]... [--work-tree=<path>] <subcommand>
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
