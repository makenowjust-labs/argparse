package codes.quine.labo.argparse

final case class VCSCommand(
    version: Boolean,
    help: Boolean,
    configurations: Seq[(String, String)],
    workTree: Option[String],
    subcommand: VCSCommand.Subcommand
)

object VCSCommand {
  val version: Quantifier[Boolean] = optional(
    switch("version", "v", "Show a verion number").map(_ => true),
    default = false
  )

  val help: Quantifier[Boolean] = optional(
    switch("help", "h", "Show this help").map(_ => true),
    default = false
  )

  val configurations: Quantifier[Seq[(String, String)]] = many(
    option[String](
      "config",
      "c",
      metavar = "<name>=<value>",
      help = "Pass a configuration parameter to command"
    ).validate {
      case s"$key=$value" => Right((key, value))
      case arg            => Left(Seq(s"wrong format: $arg"))
    }
  )

  val workTree: Quantifier[Option[String]] = optional(
    option[String](
      "work-tree",
      "",
      metavar = "<path>",
      help = "Set the path to the working tree"
    )
  )

  val subcommands: Group[Subcommand] = {
    import Subcommand._
    group("<subcommand>", Status.command <|> Add.command <|> Commit.command)
  }

  val set: ArgSet[VCSCommand] = ArgSet.map5(version, help, configurations, workTree, subcommands)(VCSCommand.apply)

  val command: Command[VCSCommand] = Command("vcs", "A version control system", set)

  sealed abstract class Subcommand

  object Subcommand {
    final case class Status(
        short: Boolean,
        paths: Seq[String]
    ) extends Subcommand

    object Status {
      val short: Quantifier[Boolean] = optional(
        switch("short", "s", "Give the output in the short-format").map(_ => true),
        default = false
      )

      val paths: Quantifier[Seq[String]] = many(positional[String]("<path>", "Paths to show status"))

      val set: ArgSet[Subcommand] = ArgSet.map2(short, paths)(Status.apply)

      val command: Arg[Subcommand] = subcommand("status", set, help = "Show the working tree status")
    }

    final case class Add(
        all: Boolean,
        paths: Seq[String]
    ) extends Subcommand

    object Add {
      val all: Quantifier[Boolean] = optional(
        switch("all", "A", "Add changes from all tracked and untracked files").map(_ => true),
        default = false
      )

      val paths: Quantifier[Seq[String]] = some(positional[String]("<path>", "Paths to add"))

      val set: ArgSet[Subcommand] = ArgSet.map2(all, paths)(Add.apply)

      val command: Arg[Subcommand] = subcommand("add", set, help = "Add file contents to the index")
    }

    final case class Commit(
        allowEmpty: Boolean,
        message: String
    ) extends Subcommand

    object Commit {
      val allowEmpty: Quantifier[Boolean] = optional(
        switch("allow-empty", "", "Allow an empty commit").map(_ => true),
        default = false
      )

      val message: Quantifier[String] = required(option[String]("message", "m", "<text>", "Set a commit message"))

      val set: ArgSet[Subcommand] = ArgSet.map2(allowEmpty, message)(Commit.apply)

      val command: Arg[Subcommand] = subcommand("commit", set, help = "Record changes to the repository")
    }
  }
}
