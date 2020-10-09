package codes.quine.labo.argparse

final case class Command[A](commandName: String, help: String, set: ArgSet[A]) {
  def parse(args: Seq[String]): Either[Seq[Failure], A] = set.parse(args)

  def toHelp: Help = toHelp(Seq.empty)

  def toHelp(names: Seq[String]): Help = {
    val subset = names.foldLeft(Option((set, help)): Option[(ArgSet[_], String)]) { (set, name) =>
      set.flatMap(_._1.subcommands.get(name))
    }
    subset
      .map { case (set, help) => set.help(commandName +: names, help) }
      .getOrElse(throw new IllegalArgumentException(s"Unknown subcommand: ${names.mkString(" ")}"))
  }
}
